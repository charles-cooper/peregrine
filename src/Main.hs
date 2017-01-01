{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Protocol
import           Protocol.Backend.C.Base as CP hiding (compile)
import qualified Protocol.Backend.C.Base as CP
import           Protocol.Tmx.TAQ as TAQ
import           Protocol.Tmx.TAQ.C as TAQ

import Protocol.Nasdaq.ITCH.Proto as ITCH
import Protocol.Nasdaq.ITCH.Proto.C as ITCH

import           Language.C.Quote.C
import qualified Language.C.Syntax as C
import qualified Language.C.Smart as C ((+=))
import           Data.Loc

import           Language.Utils
import qualified Language.C.Utils as CU
import           Language.C.Utils (C, depends, include, noloc)
import           Language.C.Utils (topDecl, require)
import           Language.C.Utils (char, bool, ushort, uint, ulong)
import           Language.C.Lib

import Data.Monoid
import Data.List (intercalate)
import Data.Maybe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Text.PrettyPrint.Mainland (putDocLn, ppr, pretty, prettyPragma, Pretty(..))
import Text.InterpolatedString.Perl6 (q, qc)
import Data.Text as T (unpack)

import Utils

import Development.Shake
import Development.Shake.FilePath

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map as Map
import           Data.Map (Map(..))
import qualified Data.Set as Set
import           Data.Set (Set(..))

import Data.Function

----------------------------
-- DSL
----------------------------

data Projection a = Projection
  { _pproto     :: (Proto a)
  , _pmsgName   :: String
  , _pfieldName :: String
  } deriving (Eq, Ord)
instance Show (Projection a) where
  show (Projection p msg field) = show p
    ++ "\".\""
    ++ msg
    ++ "\".\""
    ++ field
    ++ "\""

data Op = Add | Mul | Div | Sub | Gt | Ge | Lt | Le | Eq
  deriving (Show, Eq, Ord)

fieldsMap :: Message a -> Map String (Field a)
fieldsMap msg = Map.fromList $ (\field -> (_name field, field)) <$> _fields msg

msgsMap :: Proto a -> Map String (Message a)
msgsMap proto = Map.fromList $ (\msg -> (_msgName msg, msg)) <$> _outgoingMessages proto

resolveProjection :: Projection a -> (Field a, Message a)
resolveProjection (Projection proto msgName fieldName) = (field, msg)
  where
    msg = Map.lookup msgName (msgsMap proto)
      & fromMaybe
        (error [qc|Message "{msgName}" not found in proto {_namespace proto}|])
    field = Map.lookup fieldName (fieldsMap msg)
      & fromMaybe
        (error [qc|Field "{fieldName}" not found in message {_msgName msg}|])

type Name = Maybe String

type Group a = [Signal a]
type NodeId  = Int

-- front-end peregrine context
data Context a = Context
  { nodeId     :: NodeId
  , nodeGroup  :: Group a
  , annotation :: Maybe String
  } deriving (Eq, Ord, Show)

-- | This has to be a newtype instead of a type
-- since cycles in type synonyms are not allowed
newtype Signal a = Signal { getAST :: ASTExp a (Context a) }
  deriving (Show, Eq, Ord)

-- annotation
type Ann = Maybe String

data ASTExp
  -- | The protocol
  proto
  -- | A variable holding state.
  -- In the frontend this is the current Group
  ctx
  -- | Zip two streams together.
  -- The output will be x `op` y for most recent values of x and y
  = ZipWith ctx Op (ASTExp proto ctx) (ASTExp proto ctx)
  -- | Merge the dependency graphs for two streams.
  -- Any listeners will be updated on update of either argument.
  | MergeExp ctx (ASTExp proto ctx) (ASTExp proto ctx)
  -- | Fold an operation over a stream
  | FoldExp ctx Op (ASTExp proto ctx)
  -- | A field within message.
  -- Basically a raw signal which comes directly from the protocol
  | ProjectExp ctx (Projection proto)
  -- | Only send updates downstream if the argument is satisfied
  | GuardExp ctx (ASTExp proto ctx) (ASTExp proto ctx)
  -- | One before current
  | LastExp ctx (ASTExp proto ctx)
  -- | A constant
  | ConstExp Constant
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

addAnnotation :: String -> ASTExp a (Context a) -> ASTExp a (Context a)
addAnnotation s ast = case ast of
  ZipWith ctx op a b -> ZipWith (setAnn ctx) op a b
  MergeExp ctx a b -> MergeExp (setAnn ctx) a b
  FoldExp ctx op a -> FoldExp (setAnn ctx) op a
  ProjectExp ctx p -> ProjectExp (setAnn ctx) p
  GuardExp ctx pred a -> GuardExp (setAnn ctx) pred a
  LastExp ctx a -> LastExp (setAnn ctx) a
  ConstExp c -> ConstExp c
  where
    setAnn (Context x y _) = Context x y (Just s)

infixr 1 @!
(@!) :: Peregrine a -> String -> Peregrine a
p @! ann = do
  sig <- p
  return $ Signal (addAnnotation ann (getAST sig))

data Constant
  = ConstInt Int
  | ConstDouble Double
  | ConstBool Bool
  deriving (Show, Eq, Ord)

-- Monad for Peregrine language. It keeps knowledge of groupBy fences
type Peregrine a = StateT Int (Reader (Group a)) (Signal a)

groupBy :: Signal a -> Peregrine a -> Peregrine a
groupBy ast next = do
  modify (+1)
  local (ast:) next

isNumericOp :: Op -> Bool
isNumericOp _ = True -- all numeric for now

inferCType :: CP.Specification a -> ASTExp a (Group a) -> C CU.GType
inferCType spec ast = case ast of
  ConstExp c -> return . CU.SimpleTy $ case c of
    ConstInt _    -> CU.int
    ConstDouble _ -> CU.double
    ConstBool _   -> CU.bool
  ProjectExp _ p -> let
    (field, _) = resolveProjection p
    in CP._mkTy spec field
  LastExp _ e -> inferCType spec e
  GuardExp _ _ _ -> todo "Don't know the type of Guard"
  FoldExp _ _ x -> inferCType spec x
  MergeExp _ x y -> do
    xty <- inferCType spec x
    yty <- inferCType spec y
    if xty == yty
      then return xty
      else error "MergeExp has two arguments of different types .."
  ZipWith _ _ x y -> do
    xty <- inferCType spec x
    yty <- inferCType spec y
    return . CU.SimpleTy $ case xty of
      CU.SimpleTy xty -> case yty of
        CU.SimpleTy yty -> if
          | xty == CU.int && yty == CU.int -> CU.int
          | xty == CU.int && yty == CU.double -> CU.double
          | xty == CU.double && yty == CU.int -> CU.double
          -- bool cases?
          | otherwise -> error "I don't know how to unify two types for ZipWith"
        _ -> error "Can't unify array types in ZipWith"
      _ -> error "Can't unify array types in ZipWith"

symbolP :: Peregrine TAQ
symbolP = do
  tsymbol <- project taq "trade" "Symbol" @! "tsymbol"
  qsymbol <- project taq "quote" "Symbol" @! "qsymbol"
  merge tsymbol qsymbol                   @! "symbol"

vwapP :: Peregrine TAQ
vwapP = do
  s <- symbolP
  groupBy s $ do
    px <- project taq "Trade" "Trade Price"
    sz <- project taq "Trade" "Trade Size"
    value  <- px *. sz
    volume <- fold Add sz
    value /. volume

sumP :: Signal a -> Peregrine a
sumP xs = fold Add xs

midpointP :: Peregrine TAQ
midpointP = do
  bid <- project taq "quote" "Bid Price" @! "bid"
  ask <- project taq "quote" "Ask Price" @! "ask"
  x   <- bid +. ask
  y   <- x /. 2
  return y                               @! "midpoint"

getNodeId :: Monad m => StateT Int m Int
getNodeId = modify (+1) >> get

peregrineCtx :: StateT Int (Reader (Group a)) (Context a)
peregrineCtx = Context <$> getNodeId <*> lift ask <*> pure Nothing

merge :: Signal a -> Signal a -> Peregrine a
merge x y = do
  ctx <- peregrineCtx
  return $ Signal (MergeExp ctx (getAST x) (getAST y))

project :: Proto a -> String -> String -> Peregrine a
project p x y = do
  ctx <- peregrineCtx
  return $ Signal (ProjectExp ctx (Projection p x y))

incompleteImplementation :: a
incompleteImplementation = error "Incomplete Num instance for (Signal a)"
instance Num (Signal a) where
  fromInteger = Signal . ConstExp . ConstInt . fromInteger
  (+)    = incompleteImplementation
  (*)    = incompleteImplementation
  abs    = incompleteImplementation
  signum = incompleteImplementation
  negate = incompleteImplementation

zipWithP :: Op -> Signal a -> Signal a -> Peregrine a
zipWithP op x y = do
  ctx <- peregrineCtx
  return $ Signal (ZipWith ctx op (getAST x) (getAST y))

fold :: Op -> Signal a -> Peregrine a
fold op x = do
  ctx <- peregrineCtx
  return $ Signal (FoldExp ctx op (getAST x))

guardP :: Signal a -> Signal a -> Peregrine a
guardP pred x = do
  ctx <- peregrineCtx
  return $ Signal (GuardExp ctx (getAST pred) (getAST x))

lastP :: Signal a -> Peregrine a
lastP x = do
  ctx <- peregrineCtx
  return $ Signal (LastExp ctx (getAST x))

infixl 8 ==.
infixl 7 <.
infixl 7 >.
infixl 7 <=.
infixl 7 >=.
infixl 6 +.
infixl 6 -.
infixl 5 /.
infixl 5 *.

(==.) :: Signal a -> Signal a -> Peregrine a
(==.) = zipWithP Eq

(>.) :: Signal a -> Signal a -> Peregrine a
(>.) = zipWithP Gt

(>=.) :: Signal a -> Signal a -> Peregrine a
(>=.) = zipWithP Ge

(<.) :: Signal a -> Signal a -> Peregrine a
(<.) = zipWithP Lt

(<=.) :: Signal a -> Signal a -> Peregrine a
(<=.) = zipWithP Le

(+.) :: Signal a -> Signal a -> Peregrine a
(+.) = zipWithP Add

(-.) :: Signal a -> Signal a -> Peregrine a
(-.) = zipWithP Sub

(/.) :: Signal a -> Signal a -> Peregrine a
(/.) = zipWithP Div

(*.) :: Signal a -> Signal a -> Peregrine a
(*.) = zipWithP Mul

-- Data type which represents all the state a signal needs.
-- It will compile down to a struct which might have nested
-- hashtables pointing to other SignalState structs.
data SignalState
  = Simple CU.GType
  | Struct [SignalState]
  | SigGroup CU.GType SignalState

data CompInfo p = CompInfo
  { src      :: (Message p -> C.Exp)
  , deps     :: Set (Message p)
  , handlers :: HandleCont p -- [CP.MsgHandler p]
  }

-- notes: when we hit a fence, ensure that the source signal for the grouping
-- has been compiled. then compile the groupby update (basically insert
-- if not in the map)

type Handle a = Message a -> [C.Stm]
type HandleCont a = Message a -> (Message a -> [C.Stm]) -> [C.Stm]

runHandleCont :: HandleCont a -> Message a -> [C.Stm]
runHandleCont k m = (k m) (const [])

noop :: C.Stm
noop = [cstm|/*no-op*/(void)0;|]

data CompState a = CompState
  { nodes  :: Map NodeId (CompInfo a)
  , groups :: Map (Group a) (CompInfo a)
  }

-- Type for the Peregrine compiler
type PeregrineC a = StateT (CompState a) C

type AST a = ASTExp a (Context a)

compileConstant :: HandleCont a -> Constant -> PeregrineC a (CompInfo a)
compileConstant handler c = do
  return $ CompInfo (const value) Set.empty handler
  where
    value = case c of
      ConstInt    i -> [cexp|$i|]
      ConstDouble d -> [cexp|$d|]
      ConstBool   b -> CU.boolExp b

todo :: String -> a
todo = error . ("TODO: " ++)

traceWith :: (a -> String) -> a -> a
traceWith f a = unsafePerformIO $ putStrLn (f a) >> return a

trace :: Show a => String -> a -> a
trace s = traceWith $ ((s++": ")++) . show

class (Ord a, Show a) => Constraints a where

instance Constraints TAQ

compileZipWith :: Constraints a
  => CP.Specification a
  -> HandleCont a
  -> Context a
  -> Op
  -> AST a
  -> AST a
  -> PeregrineC a (CompInfo a)
compileZipWith spec handler ctx op x y = do

  myId <- genLocalId "zip" ctx
  (CompInfo group _ handler)    <- groupInfo spec handler (nodeGroup ctx)
  (CompInfo ref1 deps1 handler) <- compileSignal spec handler x
  (CompInfo ref2 deps2 handler) <- compileSignal spec handler y

  let
    zipExp msg = compileOp op (ref1 msg) (ref2 msg)
    out msg    = if msg `Set.member` deps
      then withCtx (group msg) myId
      else error $ "Internal invariant violated: "
        ++ "Tried to access zip from a message it doesn't depend on!"
    deps       = deps1 <> deps2

  return $ CompInfo out deps $ extendHandler handler $ \msg next ->
    if msg `Set.member` deps
      -- don't need to persist if the deps
      -- for the two signals are equal
      then [cstms|
        $(out msg) = $(zipExp msg);
        $stms:(next msg)
        |]
      else next msg

compileMerge :: Constraints a
  => CP.Specification a
  -> HandleCont a
  -> Context a
  -> AST a
  -> AST a
  -> PeregrineC a (CompInfo a)
compileMerge spec handler ctx x y = do

  myId <- genLocalId "merge" ctx
  (CompInfo group _ handler)    <- groupInfo spec handler (nodeGroup ctx)
  (CompInfo ref1 deps1 handler) <- compileSignal spec handler x
  (CompInfo ref2 deps2 handler) <- compileSignal spec handler y

  let
    out msg    = if msg `Set.member` deps
      then withCtx (group msg) myId
      else error $ "Internal invariant violated: "
        ++ "Tried to access zip from a message it doesn't depend on!"
    deps       = deps1 <> deps2
  return $ CompInfo out deps $ extendHandler handler $ \msg next ->
    -- if it's in both deps then it will default to the left
    if msg `Set.member` deps1
      then [cstms|
        $(out msg) = $(ref1 msg);
        $stms:(next msg)
        |]
      else if msg `Set.member` deps2
        then [cstms|
          $(out msg) = $(ref2 msg);
          $stms:(next msg)
          |]
        else next msg


compileProjection :: Constraints a
  => CP.Specification a
  -> HandleCont a
  -> Context a
  -> Projection a
  -> PeregrineC a (CompInfo a)
compileProjection spec handler ctx p = do

  myId <- genLocalId "projection" ctx
  (CompInfo group _ handler) <- groupInfo spec handler (nodeGroup ctx)

  let
    out msg       = withCtx (group msg) myId
    deps          = Set.singleton pmsg
    cprojection   = [cexp|msg.$id:(cnm $ _msgName pmsg).$id:fieldName|]
    (field, pmsg) = resolveProjection p
    fieldName     = cnm $ _name field

  return $ CompInfo out deps $ extendHandler handler $ \msg next ->
    if msg == pmsg
      then [cstms|
        $(out msg) = $cprojection;
        $stms:(next msg)
        |]
      else next msg

compileGuard :: Constraints a
  => CP.Specification a
  -> HandleCont a
  -> Context a
  -> AST a
  -> AST a
  -> PeregrineC a (CompInfo a)
compileGuard spec handler ctx pred ast = do
  (CompInfo group _ handler)      <- groupInfo spec handler (nodeGroup ctx)
  (CompInfo predRef deps handler) <- compileSignal spec handler pred
  (CompInfo src deps handler)     <- compileSignal spec handler ast
  -- Return the original info but wrapped in a big if
  return $ CompInfo src deps $ extendHandler handler $ \msg next ->
    if msg `Set.member` deps
      then [cstms|
         if ($(predRef msg)) {
           $stms:(next msg)
         }
       |]
      else next msg


topState :: C.Exp
topState = [cexp|state|]

groupInfo :: Constraints a
  => CP.Specification a
  -> HandleCont a
  -> Group a
  -> PeregrineC a (CompInfo a)
groupInfo spec handler group = case group of
  []  -> return noGroup
  g:_ -> do
    st <- groups <$> get
    case Map.lookup group st of
      Just t  -> return t -- we've already compiled the group
      Nothing -> do
        ret <- genGroupInfo (getAST g)
        modify (\t -> t { groups = Map.insert group ret st })
        return ret

  where
    noGroup = CompInfo (const topState) mempty handler
    genGroupInfo g = do
      gid    <- lift $ CU.idExp <$> CU.genId "group"
      (CompInfo key deps handler) <- compileSignal spec handler g

      let
        iter    = "dummy_iter"
        iterptr = "dummy_iter_ptr"
        map     = "dummy_map"
        value   = [cexp|dummy_initializer|]

        h       = extendHandler handler $ \msg next ->
          if msg `Set.member` deps
            then [cstms|
              $gid = $id:map.insert($(key msg), $value).first->second;
              $stms:(next msg)
              |]
            else next msg

      return CompInfo
        { src      = const gid
        , deps     = error
          -- A change in the group by deps shouldn't cascade to those who
          -- are depending only on the groupby
          -- e.g. groupBy trade.symbol (something-depends-on-quote)
          -- shouldn't update when trade.symbol is updated
          "Internal invariant violated: Illegally accessed groupBy deps!"
        , handlers = h
        }

extendHandler :: HandleCont a -> HandleCont a -> HandleCont a
extendHandler handler cont = \msg next -> handler msg $ \msg -> cont msg next

compileLast :: Constraints a
  => CP.Specification a
  -> HandleCont a -- idk.
  -> Context a
  -> AST a
  -> PeregrineC a (CompInfo a)
compileLast spec handler ctx src = do
  myId <- genLocalId "last" ctx
  (CompInfo group _ handler) <- groupInfo spec handler (nodeGroup ctx)
  (CompInfo refFromMsg deps handler) <- compileSignal spec handler src
  let
    out m = withCtx (group m) myId
  return $ CompInfo out deps $ extendHandler handler $ \msg next ->
    if msg `Set.member` deps
      then [cstms|
          $stms:(next msg)
          $(out msg) = $exp:(refFromMsg msg);
        |]
      else next msg

genLocalId :: String -> Context a -> PeregrineC a String
-- TODO: generate from a new state for every fence
genLocalId default_ ctx = lift (CU.genId (cnm id))
  where
    id = case annotation ctx of
      Nothing -> default_
      Just s  -> s

compileFold :: Constraints a
  => CP.Specification a
  -> HandleCont a
  -> Context a
  -> Op
  -> AST a
  -> PeregrineC a (CompInfo a)
compileFold spec handler ctx op src = do
  myId <- genLocalId "fold" ctx
  (CompInfo group _ handler) <- groupInfo spec handler (nodeGroup ctx)
  (CompInfo refFromMsg deps handler) <- compileSignal spec handler src
  let
    out m = withCtx (group m) myId
  return $ CompInfo out deps $ extendHandler handler $ \msg next ->
    let exp msg = compileOp op (out msg) (refFromMsg msg)
    in if msg `Set.member` deps
        then [cstms|
           $(out msg) = $(exp msg);
           $stms:(next msg)
         |]
        else next msg

withCtx :: C.Exp -> String -> C.Exp
withCtx ctx x = [cexp|$ctx -> $id:x|]

compileOp :: Op -> C.Exp -> C.Exp -> C.Exp
compileOp Add x y = [cexp|$x + $y|]
compileOp Sub x y = [cexp|$x - $y|]
compileOp Mul x y = [cexp|$x * $y|]
compileOp Div x y = [cexp|$x / $y|]
compileOp Gt x y  = [cexp|$x > $y|]
compileOp Ge x y  = [cexp|$x >= $y|]
compileOp Lt x y  = [cexp|$x < $y|]
compileOp Le x y  = [cexp|$x <= $y|]
compileOp Eq x y  = [cexp|$x == $y|]

-- Convert all the high-level Group annotations into low-level
-- C identifiers
{-
genCtx :: Constraints a => ASTExp a (NodeId, Maybe (Signal a)) -> C (AST a)
genCtx ast = evalStateT (mapM step ast) (Nothing, Map.empty)
  where
    step (nid, mgroup) = do
      (toplevel, st) <- get
      case mgroup of
        Nothing -> do -- we are at the toplevel
          case toplevel of
            Nothing -> do
              -- first time we have seen the toplevel state
              id <- lift (CU.idExp <$> CU.genId [qc|state|])
              put (Just id, st)
              return Context
                { nodeId  = nid
                , fence   = True
                , groupId = id
                , group   = mgroup
                }
            Just cid -> do
              return $ Context
                { nodeId  = nid
                , fence   = False
                , groupId = cid
                , group   = mgroup
                }
        Just g  -> do
          case Map.lookup g st of
            Nothing -> do
              -- first time we have seen this groupBy fence.
              id <- lift (CU.idExp <$> CU.genId [qc|group_by|])
              put (toplevel, Map.insert g id st)
              return Context
                { nodeId  = nid
                , fence   = True
                , groupId = id
                , group   = mgroup
                }
            Just cid -> do
              return Context
                { nodeId  = nid
                , fence   = False
                , groupId = cid
                , group = mgroup
                }
-}

-- This works because ASTExp constructors have one or zero `ctx`s
getCtx :: ASTExp a ctx -> Maybe ctx
getCtx = foldr (\a b -> Just a) Nothing

-- TODO need to build up the groupBy data structure definitions as we go

compileSignal :: Constraints a
  => CP.Specification a
  -> HandleCont a
  -> AST a
  -> PeregrineC a (CompInfo a)
compileSignal spec handler ast = case ast of
  FoldExp ctx op ast -> compileOnce (nodeId ctx) $ do
    compileFold spec handler ctx op ast
  ZipWith ctx op ast1 ast2 -> compileOnce (nodeId ctx) $ do
    compileZipWith spec handler ctx op ast1 ast2
  MergeExp ctx ast1 ast2 -> compileOnce (nodeId ctx) $ do
    compileMerge spec handler ctx ast1 ast2
  ProjectExp ctx p -> compileOnce (nodeId ctx) $ do
    compileProjection spec handler ctx p
  LastExp ctx ast -> do
    compileLast spec handler ctx ast
  GuardExp ctx pred ast -> compileOnce (nodeId ctx) $ do
    compileGuard spec handler ctx pred ast
  ConstExp c -> compileConstant handler c
  where
    -- come up with a better name. basically, only compile the node if it
    -- hasn't been compiled yet, otherwise just return its existing compilation.
    compileOnce nid compilation = do
      mexists <- Map.lookup nid . nodes <$> get
      case mexists of
        Just compInfo -> return compInfo
        Nothing       -> do
          compInfo <- compilation
          modify $ \st -> st { nodes = Map.insert nid compInfo (nodes st) }
          return compInfo

{-
sumGroupBy :: AST a -> AST a -> Peregrine a -> Peregrine a
sumGroupBy group sig = do
  grouped <- groupBy group $ do
    s <- sig
    diff s
  sum grouped

perSymbolOutstanding :: Peregrine a
perSymbolOutstanding = do
  s   <- symbol
  oid <- orderId
  px  <- orderPx
  sz  <- orderSz
  sumGroupBy symbol $ do
    groupBy oid $ do
      px `mul` sz

perSymbolShares :: Peregrine a
perSymbolShares = do
  s   <- symbol
  oid <- orderId
  sz  <- orderSz
  sumGroupBy oid $ do
    groupBy symbol $ do
      return sz
-}

-- generate the dependency graph for groupBys

auto :: C.Type
auto = [cty|typename $id:("auto")|]

{-
compileGroupBy :: Ord a
  => Name
  -> ASTState a
  -> ASTState a
  -> C (ASTState a)
compileGroupBy nm
 (ASTState key   kty deps1 handlers1)
 (ASTState value vty deps2 handlers2) = do
  map  <- mkMap
  iterptr <- mkIter        -- the raw reference
  let iter = unptr iterptr -- carry around a dereferencing star
  return $ ASTState (idexp iter) iterTy deps $ snoc (handlers1<>handlers2) $ CP.MsgHandler
    { _handleMsg  = handleMsg map iter iterptr deps
    , _initMsg    = mempty_
    , _cleanupMsg = mempty_
    }
  where
    deps    = deps1 <> deps2
    mkMap   = CU.genId (maybe [qc|group_by|] id nm)
    unptr x = "*" ++ x
    mkIter  = CU.genId $ (maybe [qc|group_by|] id nm) ++ "_iter"
    -- | TODO extend cpp_unordered_map to deal with GTypes
    mapTy   = cpp_unordered_map (CU.simplety kty) (CU.simplety vty)
    iterTy  = vty
    handleMsg map iter iterptr deps msg = do
      mapty  <- mapTy
      topDecl [cdecl|$ty:mapty $id:map;|]
      topDecl (declare iter iterTy)
      -- TODO abstract the dependency checking to a higher level
      if (msg `Set.member` deps)
        then return [cstms|
          /*this can be done slightly more efficiently with `insert`*/
          if (! $id:map.count($key)) {
            $id:map[$key] = $value;
          }
          $id:iterptr = &$id:map[$key];
          $id:iter    = $value;
          |]
        else return []

declare :: String -> CU.GType -> C.InitGroup
declare id gty = case gty of
  CU.SimpleTy t   -> [cdecl|$ty:t $id:id;|]
  CU.ArrayTy t sz -> [cdecl|$ty:t $id:id[$sz];|]

main = do

  shakeArgs shakeOptions $ do

    compileShake False "bin" "runTmx" program
    compileShake False "bin" "runItch" itchProgram

    "data/*TradesAndQuotesDaily.lz4" %> \out -> do
      let src = "/mnt/efs/ftp/tmxdatalinx.com" </> (takeFileName out) -<.> "gz"
      need [src]
      command [Shell] [qc|gzip -d < {src} | lz4 -9 > {out}|] []

    phony "Run runTmx" $ do
      let date = "20150826"
      let dataFile = [qc|data/{date}TradesAndQuotesDaily.lz4|]
      let exe = "bin/runTmx"
      need [dataFile, exe]
      command_ [Shell] [qc|lz4 -d < {dataFile} | {exe} {date} |] []

    "data/*NASDAQ_ITCH50.lz4" %> \out -> do
      let src = "/mnt/efs/itch" </> (takeFileName out) -<.> "gz"
      need [src]
      command [Shell] [qc|gzip -d < {src} | lz4 -9 > {out}|] []

    phony "Run runItch" $ do
      let date = "12302015"
      let dataFile = [qc|data/{date}.NASDAQ_ITCH50.lz4|]
      let exe = "bin/runItch"
      need [dataFile, exe]
      command_ [Shell] [qc|lz4 -d < {dataFile} | {exe} {date} |] []

    -- want ["Run runItch"]
    want ["bin/runItch"]
    want ["bin/runTmx"]
-}

weightedMidpointP :: Peregrine TAQ
weightedMidpointP = do
  bidpx  <- project taq "quote" "Bid Price" @! "bidpx"
  bidsz  <- project taq "quote" "Bid Size"  @! "bidsz"
  askpx  <- project taq "quote" "Ask Price" @! "askpx"
  asksz  <- project taq "quote" "Ask Size"  @! "asksz"
  bidval <- bidpx *. bidsz                  @! "bidval"
  askval <- askpx *. asksz                  @! "askval"
  totsz  <- bidsz +. asksz                  @! "totsz"
  tmpsum <- bidval +. askval                @! "tmp"
  tmpsum /. totsz                           @! "weighted midpoint"

midpointSkew :: Peregrine TAQ
midpointSkew = do
  symbol <- project taq "quote" "Symbol" @! "symbol" -- symbolP
  groupBy symbol $ do
    normalMid <- midpointP               @! "midpoint"
    -- weightMid <- weightedMidpointP @! "weighted midpoint"
    normalMid -. 1                       @! "midpoint skew"

simpleProgram :: Peregrine TAQ
simpleProgram = do
  symbol <- project taq "trade" "Symbol"
  groupBy symbol $ do
    midp <- midpointP
    bid  <- project taq "quote" "Bid Price" @! "bidpx"
    lbid <- lastP bid                       @! "lastbid"
    sum  <- sumP lbid                       @! "sumbid"
    pred <- sum >. 0                        @! "pred"
    twice_sum <- sum +. sum                 @! "twice sum"
    x <- guardP pred twice_sum
    sum /. midp                             @! "weird quantity"

runPeregrine :: Peregrine a -> Signal a
runPeregrine peregrine = runReader (evalStateT peregrine 0) []

runIntermediate :: Constraints a => PeregrineC a b -> C b
runIntermediate p = evalStateT p (CompState mempty mempty)

msgHandler :: Constraints a => Specification a -> Peregrine a -> C (CP.MsgHandler a)
msgHandler spec peregrine = do

  let iast = getAST (runPeregrine peregrine)
  (CompInfo _ _ cont) <- runIntermediate (compileSignal spec (\msg cont -> cont msg) iast)
  return $ CP.MsgHandler (\msg -> return (runHandleCont cont msg)) empty empty
  where
    empty = const (pure [noop])

-- just generates the message handler for inspection / debugging
mkHandler :: Peregrine TAQ -> C C.Func
mkHandler prog = mainLoop taqCSpec =<< msgHandler taqCSpec prog

-- This program is a problem because multiple nodes refer to
-- the `bid` node and so it will get compiled twice.
problem :: Peregrine TAQ
problem = do
  bid <- project taq "quote" "Bid Price"
  y <- bid +. bid
  bid +. y

main = do
  putStrLn $ CP.compile False (mkHandler midpointSkew)


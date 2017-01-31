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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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
import qualified Language.C.Lib as CL

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

type AssocList a b = [(a, b)]

fieldsMap :: Message a -> AssocList String (Field a)
fieldsMap msg = (\field -> (_name field, field)) <$> _fields msg

msgsMap :: Proto a -> AssocList String (Message a)
msgsMap proto = (\msg -> (_msgName msg, msg)) <$> _outgoingMessages proto

resolveProjection :: Projection a -> (Field a, Message a)
resolveProjection (Projection proto msgName fieldName) = (field, msg)
  where
    msg   = lookup msgName (msgsMap proto)
      & fromMaybe
        (error [qc|Message "{msgName}" not found in proto {_namespace proto}|])
    field = lookup fieldName (fieldsMap msg)
      & fromMaybe
        (error [qc|Field "{fieldName}" not found in message {_msgName msg}|])

data Constant
  = ConstInt Int
  | ConstDouble Double
  | ConstBool Bool
  deriving (Show, Eq, Ord)

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

-- | This has to be a newtype instead of a type
-- since cycles in type synonyms are not allowed
newtype Signal a = Signal { getAST :: ASTExp a (Context a) }
  deriving (Show, Eq, Ord)

type Group a = [Signal a]
type NodeId  = Int

-- front-end peregrine context
data Context a = Context
  { nodeId     :: NodeId
  , nodeGroup  :: Group a
  , annotation :: Maybe String
  } deriving (Eq, Ord, Show)

incompleteImplementation :: a
incompleteImplementation = error "Incomplete Num instance for (Signal a)"
instance Num (Signal a) where
  fromInteger = Signal . ConstExp . ConstInt . fromInteger
  (+)    = incompleteImplementation
  (*)    = incompleteImplementation
  abs    = incompleteImplementation
  signum = incompleteImplementation
  negate = incompleteImplementation

instance Num (Peregrine a) where
  fromInteger = return . fromInteger
  a + b       = join $ (+.) <$> a <*> b
  a * b       = join $ (*.) <$> a <*> b
  abs    = incompleteImplementation
  signum = incompleteImplementation
  negate = incompleteImplementation

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

-- Set the C variable name programmatically
infixr 1 @!
(@!) :: Peregrine a -> String -> Peregrine a
p @! ann = do
  sig <- p
  return $ Signal (addAnnotation ann (getAST sig))

-- Monad for Peregrine language. It keeps knowledge of groupBy fences
type Peregrine a = StateT Int (Reader (Group a)) (Signal a)

groupBy :: Signal a -> Peregrine a -> Peregrine a
groupBy ast next = do
  modify (+1)
  local (ast:) next

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
type GroupStruct = [(CU.GType, String)]

data CompInfo p = CompInfo
  { src      :: String
  , ref      :: (Message p -> C.Exp)
  , ctype    :: CU.GType
  , deps     :: Set (Message p)
  , handlers :: HandleCont p
  }

type Handle a = Message a -> [C.Stm]
type HandleCont a = Message a -> (Message a -> [C.Stm]) -> [C.Stm]

runHandleCont :: HandleCont a -> Message a -> [C.Stm]
runHandleCont k m = (k m) (const [])

noop :: C.Stm
noop = [cstm|/*no-op*/(void)0;|]

data CompState a = CompState
  { nodes   :: Map NodeId (CompInfo a)
  , groups  :: Map (Group a) (CompInfo a, GroupStruct)
  }

-- Type for the Peregrine compiler
type PeregrineC a = StateT (CompState a) C

type AST a = ASTExp a (Context a)

class (Ord a, Show a) => Constraints a where

instance Constraints TAQ

todo :: String -> a
todo = error . ("TODO: " ++)

traceWith :: (a -> String) -> a -> a
traceWith f a = unsafePerformIO $ putStrLn (f a) >> return a

trace :: Show a => String -> a -> a
trace s = traceWith $ ((s++": ")++) . show

genLocalId :: String -> Context a -> PeregrineC a String
-- TODO: generate from a new state for every fence
genLocalId default_ ctx = lift (CU.genId (cnm id))
  where
    id = case annotation ctx of
      Nothing -> default_
      Just s  -> s

extendHandler :: HandleCont a -> HandleCont a -> HandleCont a
extendHandler handler cont = \msg next -> handler msg (\msg -> cont msg next)

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

-- This works because ASTExp constructors have one or zero `ctx`s
getCtx :: ASTExp a ctx -> Maybe ctx
getCtx = foldr (\a b -> Just a) Nothing

topState :: String
topState = "state"

topStateTy :: C.Type
topStateTy = [cty|typename $id:("struct " ++ topState)|]

groupInfo :: Constraints a
  => CP.Specification a
  -> HandleCont a
  -> Group a
  -> PeregrineC a (CompInfo a)
groupInfo spec handler group = case group of
  []   -> noGroup
  g:gs -> do
    st <- groups <$> get
    case Map.lookup group st of
      -- we've already compiled the group. return the head node of the topsort
      Just (t, _) -> return t { handlers = handler }
      Nothing -> genGroupInfo (getAST g) gs

  where
    errorty = error "Can't propagate type of group!"
    ref     = const (CU.idExp topState)
    noGroup = do
      let
        compInfo = CompInfo topState ref errorty mempty handler
        go       = maybe (Just (compInfo, [])) Just
      modify (\t -> t { groups = Map.alter go group (groups t) })
      return compInfo

    genGroupInfo g gs = do
      (CompInfo _ parent _ deps handler) <- groupInfo spec handler gs
      -- the type of the struct
      groupId <- lift $ CU.genId "group_map"
      iterId  <- lift $ CU.genId "group"
      (CompInfo _ key kty deps handler) <- compileSignal spec handler g

      let
        iter    = [cty|typename $id:("struct " ++ iterId)|]
        iterPtr = [cty|typename $id:("struct " ++ iterId ++ " *")|]
        gexp  m = withCtx (parent m) groupId
        lhs   m = withCtx (parent m) iterId
        h       = extendHandler handler $ \msg next ->
          if msg `Set.member` deps
            then [cstms|
              if (! $(gexp msg).count($(key msg))) {
                $ty:iter tmp;/*TODO sane initialization*/
                $(gexp msg)[$(key msg)] = tmp;
              }
              //Set the pointer so future dereferences within this callback
              //will refer to the right thing
              $(lhs msg) = &(($(gexp msg))[$(key msg)]);
              $stms:(next msg)
              |]
            else next msg
        ret = CompInfo
          { src      = iterId
          , ref      = \msg -> (withCtx (parent msg) iterId)
          , ctype    = errorty
          , deps     = error
            -- A change in the group by deps shouldn't cascade to those who
            -- are depending only on the groupby
            -- e.g. groupBy trade.symbol (something-depends-on-quote)
            -- shouldn't update when trade.symbol is updated
            "Internal invariant violated: Illegally accessed groupBy deps!"
          , handlers = h
          }

      -- Insert self into global list of structs
      modify (\t -> t { groups = Map.insert group (ret, []) (groups t) })

      mapTy   <- lift $ CL.cpp_unordered_map (CU.simplety kty) iter
      let
        iterData = (CU.SimpleTy iterPtr, iterId)
        mapData  = (CU.SimpleTy mapTy, groupId)
        go Nothing = error "Didn't find group in map.."
        go (Just (compInfo, elems)) = Just
          (compInfo, iterData : mapData : elems)

      -- Insert self into parent struct
      modify (\t -> t { groups = Map.alter go gs (groups t) })

      return ret

compileConstant :: HandleCont a -> Constant -> PeregrineC a (CompInfo a)
compileConstant handler c = do
  return $ CompInfo src (const value) ty Set.empty handler
  where
    src = error "No variable for constant"
    (value, ty) = case c of
      ConstInt    i -> ([cexp|$i|], CU.SimpleTy CU.int)
      ConstDouble d -> ([cexp|$d|], CU.SimpleTy CU.double)
      ConstBool   b -> (CU.boolExp b, CU.SimpleTy CU.bool)

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
  (CompInfo _ group _ _ handler)    <- groupInfo spec handler (nodeGroup ctx)
  (CompInfo _ src1 ty1 deps1 handler) <- compileSignal spec handler x
  (CompInfo _ src2 ty2 deps2 handler) <- compileSignal spec handler y

  let
    zipExp msg = compileOp op (src1 msg) (src2 msg)
    out msg    = if msg `Set.member` deps
      then withCtx (group msg) myId
      else error $ "Internal invariant violated: "
        ++ "Tried to access zip from a message it doesn't depend on!"
    deps       = deps1 <> deps2
    sty1       = CU.simplety ty1
    sty2       = CU.simplety ty2
    ty         = case () of
      _ | ty1 == ty2
        -> ty1
      _ | not (CU.isNumeric sty1) || not (CU.isNumeric sty2)
        -> error $ "Can't zip types " <> show sty1 <> ", " <> show sty2
      _ | op == Div || CU.double `elem` [sty1, sty2]
        -> CU.SimpleTy CU.double
      _ | op `elem` [Add, Mul, Sub] && CU.signed sty1 == CU.signed sty2
        -> if CU.width sty1 >= CU.width sty2 then ty1 else ty2
      _ | otherwise
        -> error $ "Can't zip numeric types??" <> show sty1 <> ", " <> show sty2


  return $ CompInfo myId out ty deps $ extendHandler handler $ \msg next ->
    if msg `Set.member` deps
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
  (CompInfo _ group _ _ handler)      <- groupInfo spec handler (nodeGroup ctx)
  (CompInfo _ ref1 ty1 deps1 handler) <- compileSignal spec handler x
  (CompInfo _ ref2 ty2 deps2 handler) <- compileSignal spec handler y

  let
    out msg    = if msg `Set.member` deps
      then withCtx (group msg) myId
      else error $ "Internal invariant violated: "
        ++ "Tried to access merge from a message it doesn't depend on!"
    deps       = deps1 <> deps2
    ty         = if ty1 == ty2
      then ty1
      else error $ "Tried to merge two unequal types, I don't know what to do"
  return $ CompInfo myId out ty deps $ extendHandler handler $ \msg next ->
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
  (CompInfo _ group _ _ handler) <- groupInfo spec handler (nodeGroup ctx)

  let
    out msg       = withCtx (group msg) myId
    deps          = Set.singleton pmsg
    cprojection   = [cexp|msg.$id:(cnm $ _msgName pmsg).$id:fieldName|]
    (field, pmsg) = resolveProjection p
    fieldName     = cnm $ _name field

  ty <- lift (_mkTy spec field)

  return $ CompInfo myId out ty deps $ extendHandler handler $ \msg next ->
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
  (CompInfo _ group _ _ handler)      <- groupInfo spec handler (nodeGroup ctx)
  (CompInfo _ predRef _ deps handler) <- compileSignal spec handler pred
  (CompInfo src ref ty deps handler)  <- compileSignal spec handler ast
  -- Return the original info but wrapped in a big if
  return $ CompInfo src ref ty deps $ extendHandler handler $ \msg next ->
    if msg `Set.member` deps
      then [cstms|
         if ($(predRef msg)) {
           $stms:(next msg)
         }
       |]
      else next msg

compileLast :: Constraints a
  => CP.Specification a
  -> HandleCont a -- idk.
  -> Context a
  -> AST a
  -> PeregrineC a (CompInfo a)
compileLast spec handler ctx src = do
  myId <- genLocalId "last" ctx
  (CompInfo _ group _ _ handler)   <- groupInfo spec handler (nodeGroup ctx)
  (CompInfo _ ref ty deps handler) <- compileSignal spec handler src
  let
    out m = withCtx (group m) myId
  return $ CompInfo myId out ty deps $ extendHandler handler $ \msg next ->
    if msg `Set.member` deps
      then [cstms|

          // Do whatever needs to be done
          $stms:(next msg)

          // Finally, assign to storage
          $(out msg) = $exp:(ref msg);
        |]
      else next msg

compileFold :: Constraints a
  => CP.Specification a
  -> HandleCont a
  -> Context a
  -> Op
  -> AST a
  -> PeregrineC a (CompInfo a)
compileFold spec handler ctx op src = do
  myId <- genLocalId "fold" ctx
  (CompInfo _ group _ _ handler)   <- groupInfo spec handler (nodeGroup ctx)
  (CompInfo _ ref ty deps handler) <- compileSignal spec handler src
  let
    out m = withCtx (group m) myId
  return $ CompInfo myId out ty deps $ extendHandler handler $ \msg next ->
    let exp msg = compileOp op (out msg) (ref msg)
    in if msg `Set.member` deps
        then [cstms|
           $(out msg) = $(exp msg);
           $stms:(next msg)
         |]
        else next msg

compileSignal :: Constraints a
  => CP.Specification a
  -> HandleCont a
  -> AST a
  -> PeregrineC a (CompInfo a)
compileSignal spec handler ast = case ast of
  FoldExp ctx op ast -> compileOnce ctx $ do
    compileFold spec handler ctx op ast
  ZipWith ctx op ast1 ast2 -> compileOnce ctx $ do
    compileZipWith spec handler ctx op ast1 ast2
  MergeExp ctx ast1 ast2 -> compileOnce ctx $ do
    compileMerge spec handler ctx ast1 ast2
  ProjectExp ctx p -> compileOnce ctx $ do
    compileProjection spec handler ctx p
  LastExp ctx ast -> compileOnce ctx $ do
    compileLast spec handler ctx ast
  GuardExp ctx pred ast -> compileOnce ctx $ do
    compileGuard spec handler ctx pred ast
  ConstExp c -> compileConstant handler c
  where
    -- come up with a better name. basically, only compile the node if it
    -- hasn't been compiled yet, otherwise just return the current head
    -- of the topsort
    compileOnce (Context nid group _) compilation = do
      mexists <- Map.lookup nid . nodes <$> get
      case mexists of
        Just compInfo -> return compInfo { handlers = handler }
        Nothing       -> do
          compInfo <- compilation

          -- mark seen
          modify $ \st -> st { nodes  = Map.insert nid compInfo (nodes st) }

          -- insert its declaration into its container
          let
            structData = (ctype compInfo, src compInfo)
            go Nothing = error "Didn't find group info .. "
            go (Just (compInfo, elems)) = Just (compInfo, structData : elems)
          modify $ \st -> st { groups = Map.alter go group (groups st) }

          return compInfo

diff :: Signal a -> Peregrine a
diff sig = do
  sig' <- lastP sig
  sig -. sig' @! "diff"

sumGroupBy :: Signal a -> Peregrine a -> Peregrine a
sumGroupBy group sig = do
  grouped <- groupBy group $ do
    s <- sig
    diff s
  sumP grouped

{-
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

{-
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

symbolP :: Peregrine TAQ
symbolP = do
  tsymbol <- project taq "trade" "Symbol" @! "tsymbol"
  qsymbol <- project taq "quote" "Symbol" @! "qsymbol"
  merge tsymbol qsymbol                   @! "symbol"

vwapP :: Signal TAQ -> Peregrine TAQ
vwapP group = groupBy group $ do
  px <- project taq "Trade" "Trade Price"
  sz <- project taq "Trade" "Trade Size"
  value  <- px *. sz
  volume <- fold Add sz
  value /. volume

sumP :: Signal a -> Peregrine a
sumP xs = fold Add xs

midpointP :: Signal TAQ -> Peregrine TAQ
midpointP group = groupBy group $ do
  bid <- project taq "quote" "Bid Price" @! "bid"
  ask <- project taq "quote" "Ask Price" @! "ask"
  x   <- bid +. ask
  y   <- x /. 2
  return y                               @! "midpoint"

weightedMidpointP :: Signal TAQ -> Peregrine TAQ
weightedMidpointP group = groupBy group $ do
  bidpx  <- project taq "quote" "Bid Price" @! "bidpx"
  bidsz  <- project taq "quote" "Bid Size"  @! "bidsz"
  askpx  <- project taq "quote" "Ask Price" @! "askpx"
  asksz  <- project taq "quote" "Ask Size"  @! "asksz"
  bidval <- bidpx  *. bidsz                 @! "bidval"
  askval <- askpx  *. asksz                 @! "askval"
  totsz  <- bidsz  +. asksz                 @! "totsz"
  tmpsum <- bidval +. askval                @! "tmp"
  tmpsum /. totsz                           @! "weighted midpoint"

midpointSkew :: Signal TAQ -> Peregrine TAQ
midpointSkew group = do
  normalMid <- midpointP group         @! "midpoint"
  weightMid <- weightedMidpointP group @! "weighted midpoint"
  groupBy group $ do
    weightMid -. normalMid             @! "midpoint skew"

simpleProgram :: Signal TAQ -> Peregrine TAQ
simpleProgram group = do
  midp   <- midpointP group
  symbol <- symbolP
  groupBy symbol $ do
    bid  <- project taq "quote" "Bid Price" @! "bidpx"
    lbid <- lastP bid                       @! "lastbid"
    sum  <- sumP lbid                       @! "sumbid"
    pred <- sum >. 0                        @! "pred"
    twice_sum <- sum +. sum                 @! "twice sum"
    x <- guardP pred sum
    sum /. midp                             @! "weird quantity"

runPeregrine :: Peregrine a -> Signal a
runPeregrine peregrine = runReader (evalStateT peregrine 0) []

runIntermediate :: Constraints a => PeregrineC a b -> C (b, CompState a)
runIntermediate p = runStateT p (CompState mempty mempty)

msgHandler :: Constraints a => Specification a -> Peregrine a -> C (CP.MsgHandler a)
msgHandler spec peregrine = do

  let ast = getAST (runPeregrine peregrine)
  iast <- runIntermediate (compileSignal spec endCont ast)
  let
    (CompInfo _ _ _ _ cont) = fst iast
    (CompState _ struct)    = snd iast

  forM (reverse (Map.elems struct)) $ \(compInfo, elems) -> do
    -- TODO sort elems by C width
    cstruct (src compInfo) $ uncurry (flip CP.mkCsdecl) <$> elems

  foo <- CU.genId "foo"
  topDecl [cdecl|$ty:topStateTy $id:foo;|]
  topDecl [cdecl|$ty:topStateTy *$id:topState = &$id:foo;|]
  return $ CP.MsgHandler (\msg -> return (runHandleCont cont msg)) empty empty
  where
    empty   = const (pure [])
    endCont = \msg cont -> cont msg

-- just generates the message handler for inspection / debugging
p2c :: Peregrine TAQ -> C C.Func
p2c prog = cmain taqCSpec =<< msgHandler taqCSpec prog

-- This program represents the sharing problem because multiple nodes refer to
-- the `bid` node and so it will get compiled twice unless we modulo
-- the sharing
problem :: Peregrine TAQ
problem = do
  bid <- project taq "quote" "Bid Price"
  y <- bid +. bid
  bid +. y

zipBug :: Peregrine TAQ
zipBug = do
  s <- project taq "quote" "Symbol"
  groupBy s $ do
    bid <- project taq "quote" "Bid Price" @! "bid"
    ask <- project taq "quote" "Ask Price" @! "ask"
    x   <- bid +. ask                      @! "x"
    y   <- bid -. ask                      @! "y"
    x +. y                                 @! "bug"

marketBidVal :: Peregrine TAQ
marketBidVal = (@! "Market Bid Val") $ do
  s <- symbolP
  sumGroupBy s $ do
    bidpx <- project taq "quote" "Bid Price" @! "bid price"
    bidsz <- project taq "quote" "Bid Size"  @! "Bid size"
    bidpx *. bidsz                           @! "bid val"

groupBySymbol :: (Signal TAQ -> Peregrine TAQ) -> Peregrine TAQ
groupBySymbol signalWithGroup = do
  symbol <- symbolP
  signalWithGroup symbol

main = shakeArgs shakeOptions $ do
  CP.compileShake True "bin" "peregrine" (p2c (groupBySymbol midpointSkew))
  want ["bin/peregrine"]


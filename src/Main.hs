{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import           Protocol.Nasdaq.ITCH.Proto as ITCH
import           Protocol.Nasdaq.ITCH.Proto.C as ITCH

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

-- Actually running the compiled program
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           System.Directory
import           System.Process

import Data.Monoid
import Data.List (intercalate, tails)
import Data.Maybe
import Data.Tuple (swap)
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Identity
import Data.Fix
import Data.Ratio

import Text.PrettyPrint.Mainland (putDocLn, ppr, pretty, prettyPragma, Pretty(..))
import           Text.InterpolatedString.Perl6 (q, qc)
import           Data.String.Here.Interpolated
import           Data.String
import qualified Data.Text as T

import Utils

import Development.Shake
import Development.Shake.FilePath

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map as Map
import           Data.Map (Map(..))
import qualified Data.IntMap as IMap
import           Data.IntMap (IntMap(..), (!))
import qualified Data.Set as Set
import           Data.Set (Set(..))
import           Data.Sequence (Seq(..), (|>))
import           Data.Foldable hiding (fold)

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

data BinOp = Add | Mul | Div | Sub | Gt | Ge | Lt | Le | Eq
  deriving (Eq, Ord, Show)

data UnaryOp
  = Math String -- function name from cmath e.g. "abs"
  deriving (Eq, Ord, Show)

type AssocList a b = [(a, b)]

fieldsMap :: Message a -> AssocList String (Field a)
fieldsMap msg = (_name &&& id) <$> _fields msg

msgsMap :: Proto a -> AssocList String (Message a)
msgsMap proto = (_msgName &&& id) <$> _outgoingMessages proto

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

data Observation = Every | Summary
  deriving (Eq, Ord, Show)

data ASTF
  -- | The protocol
  proto
  -- | A variable holding state.
  -- In the frontend this is the current Group
  ctx
  -- | Recurse
  next
  -- | Zip two streams together.
  -- The output will be x `op` y for most recent values of x and y
  = ZipWith ctx BinOp next next
  -- | Merge the dependency graphs for two streams.
  -- Any listeners will be updated on update of either argument.
  | MergeExp ctx next next
  -- | Unary operation
  | MapExp ctx UnaryOp next
  -- | Fold an operation over a stream
  | FoldExp ctx BinOp next
  -- | A field within message.
  -- Basically a raw signal which comes directly from the protocol
  | ProjectExp ctx (Projection proto)
  -- | Only send updates downstream if the argument is satisfied
  | GuardExp ctx next next
  -- | One before current
  | LastExp ctx next
  -- | Observe an expression
  | ObserveExp ctx Observation next
  -- | A constant
  | ConstExp Constant
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Signal a = Fix (ASTF a (Context (Group a)))

newtype Group a = Group { getGroup :: [Signal a] }
  deriving (Eq, Ord)

type NodeID  = Int
type Nodes a ctx = IntMap (ASTF a ctx NodeID)
type IGroup = [NodeID]

-- front-end peregrine context
data Context g = Context
  { nodeGroup  :: g
  , annotation :: Maybe String
  } deriving (Show, Functor, Foldable, Traversable)
type IContext a = (Dependencies a, Context IGroup)

-- Special Eq and Ord instances for Context
-- because we don't want the annotation or generated ids
-- to have any effect on map lookups
instance Eq a => Eq (Context a) where
  Context group1 _ == Context group2 _ = group1 == group2
instance Ord a => Ord (Context a) where
  Context group1 _ `compare` Context group2 _ = group1 `compare` group2

incomplete :: a
incomplete = error "Incomplete instance"
instance Num (Signal a) where
  fromInteger = Fix . ConstExp . ConstInt . fromInteger
  (+)    = incomplete
  (*)    = incomplete
  (-)    = incomplete
  abs    = incomplete
  signum = incomplete

instance Num (Peregrine a) where
  fromInteger = return . fromInteger
  a + b       = join $ (+.) <$> a <*> b
  a * b       = join $ (*.) <$> a <*> b
  a - b       = join $ (-.) <$> a <*> b
  abs         = incomplete
  signum      = incomplete

instance Fractional (Peregrine a) where
  fromRational x = fromIntegral (numerator x) / fromInteger (denominator x)
  x / y          = join $ (/.) <$> x <*> y

addAnnotation :: String -> ASTF a (Context b) c -> ASTF a (Context b) c
addAnnotation s = mapCtx setAnn
  where
    setAnn (Context x _) = Context x (Just s)

-- Set the C variable name programmatically
infixr 1 @!
(@!) :: Peregrine a -> String -> Peregrine a
p @! ann = do
  sig <- p
  return $ Fix (addAnnotation ann (unFix sig))

-- Monad for Peregrine language. It keeps knowledge of groupBy fences
type Peregrine a = Reader (Group a) (Signal a)

groupBy :: Signal a -> Peregrine a -> Peregrine a
groupBy group next = do
  local (Group . (group:) . getGroup) next

signal :: (Context (Group a) -> Signal a) -> Peregrine a
signal ast = do
  gs <- ask
  ctx <- pure $ Context gs Nothing
  return (ast ctx)

merge :: Signal a -> Signal a -> Peregrine a
merge x y = signal $ \ctx -> Fix $ MergeExp ctx (x) (y)

project :: Proto a -> String -> String -> Peregrine a
project p x y = signal $ \ctx -> Fix $ ProjectExp ctx (Projection p x y)

zipWithP :: BinOp -> Signal a -> Signal a -> Peregrine a
zipWithP op x y = signal $ \ctx -> Fix $ ZipWith ctx op (x) (y)

foldP :: BinOp -> Signal a -> Peregrine a
foldP op x = signal $ \ctx -> Fix $ FoldExp ctx op (x)

mapP :: UnaryOp -> Signal a -> Peregrine a
mapP f x = signal $ \ctx -> Fix $ MapExp ctx f x

guardP :: Signal a -> Signal a -> Peregrine a
guardP pred x = signal $ \ctx -> Fix $ GuardExp ctx (pred) (x)

lastP :: Signal a -> Peregrine a
lastP x = signal $ \ctx -> Fix $ LastExp ctx (x)

-- TODO change the group, `observe x` should have same group as `x`
observe :: Signal a -> Peregrine a
observe x = signal $ \ctx -> Fix $ ObserveExp ctx Every x

summary :: Signal a -> Peregrine a
summary x = signal $ \ctx -> Fix $ ObserveExp ctx Summary x

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

type GroupStruct = [(CU.GType, Identifier)]

-- Flag representing whether a variable is used outside of
-- its own dependencies. If so it needs to get written to the
-- global state, otherwise it can be assigned to a tmp var.
data Storage = Temporary | Permanent
  deriving (Eq, Show)

permanent :: Storage -> Bool
permanent = (== Permanent)

temporary :: Storage -> Bool
temporary = (== Temporary)

type Fragment = String

type HandleCont a = Message a -> (Message a -> C Fragment) -> C Fragment
type Handler a = [HandleCont a]

runHandleCont :: Ord a => [HandleCont a] -> Message a -> C [C.Stm]
runHandleCont ks msg = do
  code <- go ks
  return [cstms|$escstm:(code)|]
  where
    go ks = case ks of
      []   -> return []
      k:ks -> (k msg) (\_msg -> go ks)

newtype Exp = Exp { getExp :: String }
  deriving (Eq, Ord, Monoid)
newtype Identifier = Identifier { getIdentifier :: String }
  deriving (Eq, Ord, Monoid)
newtype Type = Type { getType :: String }
  deriving (Eq, Ord)

instance Show Exp where show = getExp
instance Show Identifier where show = getIdentifier
instance Show Type where show = getType
instance IsString Exp where fromString = Exp
instance IsString Identifier where fromString = Identifier
instance IsString Type where fromString = Type

type_ :: C.Type -> Type
type_ = Type . pretty 80 . ppr

data CompInfo p = CompInfo
  { src          :: Identifier     -- ^ The raw identifier
  , ref          :: Exp            -- ^ The expression including context
  , ctype        :: CU.GType       -- ^ The C Type
  , dependencies :: Dependencies p -- ^ Set of messages which trigger an update
  , tmp          :: Bool           -- ^ Does not need to be written to storage
  , cleanup      :: HandleCont p   -- ^ What to do at the end
  , handler      :: HandleCont p   -- ^ Function to generate C code
  }

data CompState a = CompState
  -- In the future if these turn out to be not performant,
  -- we can hash or compress representation of the nodes
  { nodeInfo  :: IntMap (CompInfo a)
  -- ^ The compilation info for each node
  , groups    :: Map IGroup (Identifier, CompInfo a, GroupStruct)
  -- ^ Map from groups to structs
  , nodeOrder :: Seq (CompInfo a)
  -- ^ An ordered visiting of the nodes
  --   Include the Node so we can lookup its revdeps
  --   A nice refactor would just be a (Seq NodeID) ..
  --   would need logic to guarantee uniqueness of
  --   nodes and also reify ordinary nodes / groups
  }

-- Type for the Peregrine compiler
data CompEnv a = CompEnv
  { specification :: CP.Specification a
  -- ^ The specification for compiling the data types to C
  , nodeContext   :: Nodes a (IContext a)
  -- ^ Metadata about the nodes calculated from previous passes
  }
type PeregrineC a = StateT (CompState a) ((ReaderT (CompEnv a)) C)

class (Ord a, Show a) => Constraints a where

instance Constraints TAQ

todo :: String -> a
todo = error . ("TODO: " ++)

traceWith :: (a -> String) -> a -> a
traceWith f a = unsafePerformIO $ putStrLn (f a) >> return a

trace :: Show a => String -> a -> a
trace s = traceWith $ ((s++": ")++) . show

genLocalId :: Identifier -> Context a -> PeregrineC b Identifier
-- TODO: generate from a new state for every fence
genLocalId default_ ctx = lift . lift $ Identifier <$> CU.genId (cnm cid)
  where
    cid = maybe (getIdentifier default_) id (annotation ctx)

appendTopsort :: CompInfo a -> PeregrineC a ()
appendTopsort node = do
  modify $ \st -> st { nodeOrder = (nodeOrder st) |> node }

withCtx :: Exp -> Identifier -> Exp
withCtx ctx x = [i|${ctx}->${x}|]

compileOp :: BinOp -> Exp -> Exp -> Exp
compileOp op x y = parens $ case op of
  Add -> [i|${x} + ${y}|]
  Sub -> [i|${x} - ${y}|]
  Mul -> [i|${x} * ${y}|]
  Div -> [i|${x} / ${y}|]
  Gt  -> [i|${x} > ${y}|]
  Ge  -> [i|${x} >= ${y}|]
  Lt  -> [i|${x} < ${y}|]
  Le  -> [i|${x} <= ${y}|]
  Eq  -> [i|${x} == ${y}|]
  where
    parens e = "(" <> e <> ")"

topState :: String -- Identifier
topState = "state"

topStateTy :: C.Type -- Type
topStateTy = [cty|typename $id:("struct " ++ topState)|]

hempty :: HandleCont p
hempty = flip ($)

groupInfo :: Constraints a
  => IGroup
  -> PeregrineC a (Identifier, CompInfo a)
groupInfo group = case group of
  []   -> do
    let
      src      = Identifier topState
      ref      = Exp topState
      compInfo = CompInfo src ref errorty dempty False hempty hempty
      set      = maybe (Just (src, compInfo, [])) Just
    modify (\t -> t { groups = Map.alter set group (groups t) })
    appendTopsort compInfo
    return (src, compInfo)

  g:gs -> do
    st <- groups <$> get
    case Map.lookup group st of
      -- we've already compiled the group
      Just (i, t, _) -> return (i, t)
      Nothing -> do
        (mapName, compInfo) <- genGroupInfo g gs
        appendTopsort compInfo
        return (Identifier mapName, compInfo)

  where
    errorty = error "Can't propagate type of group!"

    genGroupInfo g gs = do
      parent <- ref . snd <$> groupInfo gs
      -- the type of the struct
      groupId <- lift . lift $ CU.genId "group_map"
      iterId  <- lift . lift $ CU.genId "group"
      g <- compileAST g

      let
        key     = ref g
        kty     = ctype g
        deps    = dependencies g
        iter    = [cty|typename $id:("struct " ++ iterId)|]
        iterPtr = [cty|typename $id:("struct " ++ iterId ++ " *")|]
        map_    = withCtx parent (Identifier groupId)
        lhs     = withCtx parent (Identifier iterId)
        h       = \msg next -> if msg `Set.member` (_deps deps)
          then next msg >>= \next -> return [iTrim|
            if (! ${map_}.count(${key})) {
              ${type_ iter} tmp;/*TODO sane initialization*/
              ${map_}[${key}] = tmp;
            }
            //Set the pointer so future dereferences within this callback
            //will refer to the right thing
            ${lhs} = &((${map_})[${key}]);
            ${next}
            |]
          else next msg

        ret = CompInfo
          { src          = (Identifier iterId)
          , ref          = withCtx parent (Identifier iterId)
          , ctype        = errorty
          , dependencies = error "Can't access Group deps"
          , tmp          = False
          , handler      = h
          , cleanup      = hempty
          }

      mapTy <- lift . lift $ CL.cpp_unordered_map (CU.simplety kty) iter

      let
        iterData = (CU.SimpleTy iterPtr, Identifier iterId)
        mapData  = (CU.SimpleTy mapTy, Identifier groupId)
        set Nothing = error "Didn't find group in map.."
        set (Just (i, compInfo, elems)) = Just
          (i, compInfo, iterData : mapData : elems)

      -- Insert self into global list of structs
      modify (\t -> t
        { groups = Map.insert group (Identifier groupId, ret, []) (groups t) })
      -- Insert self into parent struct
      modify (\t -> t { groups = Map.alter set gs (groups t) })

      return (groupId, ret)

compileConstant :: Constant -> CompInfo a
compileConstant c = CompInfo src value ty dempty tmp hempty hempty
  where
    src         = error "No variable for constant"
    tmp         = True
    (value, ty) = first Exp $ case c of
      ConstInt    i -> (show i, CU.SimpleTy CU.int)
      ConstDouble d -> (show d, CU.SimpleTy CU.double)
      ConstBool   b -> (showB b, CU.SimpleTy CU.bool)
      where
        showB b = if b then "true" else "false"

compileZipWith :: Constraints a
  => IContext a
  -> BinOp
  -> CompInfo a
  -> CompInfo a
  -> PeregrineC a (CompInfo a)
compileZipWith (deps, ctx) op x y = do

  myId  <- genLocalId "zip" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)

  let
    ann        = maybe "" (\x -> "/*"++x++"*/") (annotation ctx)
    zipExp     = fromString ann <> compileOp op (ref x) (ref y)
    storage    = storageRequirement deps
    tmp        = temporary storage
    out        = if tmp
      then [i|${type_ (CU.simplety ty)}(${zipExp})|]
      -- ^ ugly cast. Using SSA (const temp variables)
      --   will result in clearer code
      else withCtx group myId
    sty1       = CU.simplety (ctype x)
    sty2       = CU.simplety (ctype y)
    ty         = if
      | (ctype x) == (ctype y)
        -> (ctype x)
      | not (CU.isNumeric sty1) || not (CU.isNumeric sty2)
        -> error $ "Can't zip types " <> show sty1 <> ", " <> show sty2
      | op == Div || CU.double `elem` [sty1, sty2]
        -> CU.SimpleTy CU.double
      | op `elem` [Add, Mul, Sub] && (CU.signed sty1 || CU.signed sty2)
        -> CU.SimpleTy . CU.signedOf $
          if CU.width sty1 >= CU.width sty2 then sty1 else sty2
      | op `elem` [Add, Mul, Sub] -- neither is signed
        -> CU.SimpleTy (if CU.width sty1 >= CU.width sty2 then sty1 else sty2)
      | op `elem` [Gt, Ge, Lt, Le, Eq]
        -> CU.SimpleTy CU.bool
      | otherwise
        -> error $ "Can't zip numeric types??" <> show sty1 <> ", " <> show sty2

  return $ CompInfo myId out ty deps tmp hempty $ \msg next ->
    if msg `Set.member` (_deps deps) && (not tmp)
      then next msg >>= \next -> return [iTrim|
        ${out} = ${zipExp};
        ${next}
        |]
      else next msg

compileMap :: Constraints a
  => IContext a
  -> UnaryOp
  -> CompInfo a
  -> PeregrineC a (CompInfo a)
compileMap (deps, ctx) op x = do

  myId  <- genLocalId "fmap" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)

  let
    ty      = CU.SimpleTy CU.double -- TODO revisit
    storage = storageRequirement deps
    tmp     = temporary storage
    mapExp  = case op of
      Math f -> [i|${Identifier f}(${ref x})|]
    out     = if tmp
      then mapExp
      else withCtx group myId

  return $ CompInfo myId out ty deps tmp hempty $ \msg next ->
    if msg `Set.member` (_deps deps) && (not tmp)
      then next msg >>= \next -> return [iTrim|
        ${out} = ${mapExp};
        ${next}
        |]
      else next msg

compileMerge :: Constraints a
  => IContext a
  -> CompInfo a
  -> CompInfo a
  -> PeregrineC a (CompInfo a)
compileMerge (deps, ctx) x y = do

  myId <- genLocalId "merge" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)

  let
    out        = withCtx group myId
    ty         = if ctype x == ctype y
      then ctype x
      else error $ "Tried to merge two unequal types, I don't know what to do"
    tmp        = False -- TODO revisit

  return $ CompInfo myId out ty deps tmp hempty $ \msg next -> do
    next <- next msg
    -- if it's in both deps then it will default to the left
    return $ if
      | msg `Set.member` (_deps (dependencies x)) -> [iTrim|
        ${out} = ${ref x};
        ${next}
        |]
      | msg `Set.member` (_deps (dependencies y)) -> [iTrim|
          ${out} = ${ref y};
          ${next}
          |]
      | otherwise -> next

storageRequirement :: Ord a => Dependencies a -> Storage
storageRequirement (Deps deps revdeps mustStore) = if
  | mustStore                     -> Permanent
  | revdeps `Set.isSubsetOf` deps -> Temporary
  | otherwise                     -> Permanent

compileProjection :: Constraints a
  => IContext a
  -> Projection a
  -> PeregrineC a (CompInfo a)
compileProjection (deps, ctx) p = do
  myId  <- genLocalId "projection" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)
  spec  <- specification <$> ask

  let
    storage       = storageRequirement deps
    tmp           = temporary storage
    out           = if tmp
      then cprojection
      else withCtx group myId
    cprojection   = [i|msg.${msgName}.${fieldName}|]
    (field, pmsg) = resolveProjection p
    msgName       = Identifier . cnm $ _msgName pmsg
    fieldName     = Identifier . cnm $ _name field

  ty <- lift . lift $ _mkTy spec field

  return $ CompInfo myId out ty deps tmp hempty $ \msg next ->
    if msg == pmsg && permanent storage
      then next msg >>= \next -> return [iTrim|
        ${out} = ${cprojection};
        ${next}
        |]
      else next msg

-- TODO this needs to be reasoned about more.
-- Can something from a guard escape its guard scope? (probably should be able to)
-- How does it interact with dependencies?
compileGuard :: Constraints a
  => IContext a
  -> CompInfo a
  -> CompInfo a
  -> PeregrineC a (CompInfo a)
compileGuard (deps, ctx) pred ast = do
  myId <- genLocalId "guard" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)
  pred  <- pure (ref pred) -- TODO add check that pred is type bool
  ref   <- pure (ref ast)
  ty    <- pure (ctype ast)
  let
    out = withCtx group myId
    tmp = False -- TODO can do better?

  return $ CompInfo myId out ty deps tmp hempty $ \msg next ->
    if msg `Set.member` (_deps deps)
      then next msg >>= \next -> return [iTrim|
         if (${pred}) {
           ${out} = ${ref};
           ${next}
         }
       |]
      else next msg

compileLast :: Constraints a
  => IContext a
  -> CompInfo a
  -> PeregrineC a (CompInfo a)
compileLast (deps, ctx) src = do
  myId  <- genLocalId "last" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)
  ref   <- pure (ref src)
  ty    <- pure (ctype src)
  let
    out = withCtx group myId
    tmp = False

  return $ CompInfo myId out ty deps tmp hempty $ \msg next ->
    if msg `Set.member` (_deps deps)
      then next msg >>= \next -> return [iTrim|
          // Do whatever depends on the last value
          ${next}
          // Finally, assign to storage
          ${out} = ${ref};
        |]
      else next msg

compileFold :: Constraints a
  => IContext a
  -> BinOp
  -> CompInfo a
  -> PeregrineC a (CompInfo a)
compileFold (deps, ctx) op src = do
  myId <- genLocalId "fold" ctx
  group <- ref . snd <$> groupInfo (nodeGroup ctx)
  ref <- pure (ref src)
  ty  <- pure (ctype src)
  let
    out = withCtx group myId
    tmp = False
    comment = maybe "" ("// update "++) (annotation ctx)

  return $ CompInfo myId out ty deps tmp hempty $ \msg next ->
    let exp msg = compileOp op out ref
    in if msg `Set.member` (_deps deps)
         then next msg >>= \next -> return [iTrim|
           ${comment}
           ${out} = ${exp msg};
           ${next}
           |]
         else next msg

compileObserve :: Constraints a
  => IContext a
  -> Observation
  -> CompInfo a
  -> PeregrineC a (CompInfo a)
compileObserve (deps, ctx) observeType x = do
  group     <- ref . snd <$> groupInfo (nodeGroup ctx)
  nodes     <- nodeContext <$> ask
  spec      <- specification <$> ask
  groupKeys <- mapM compileAST (nodeGroup ctx)
  groups    <- mapM groupInfo (tails (nodeGroup ctx))

  let
    tmp = True
    fmt :: CU.GType -> Exp -> C (Exp, Exp)
    fmt (CU.SimpleTy ty) e = do
      sym <- TAQ.symbol
      return $ if
        | ty == sym       -> ("%.8s", [i|(char *)(&(${e}))|])
        | ty == CU.double -> ("%f", e)
        | ty == CU.long   -> ("%ld", e)
        | ty == CU.ulong  -> ("%lu", e)
        | CU.signed ty    -> ("%d", e)
        | otherwise       -> ("%u", e)
    fmt (CU.ArrayTy {}) _ = error "TODO format array ty"
    toPrint = map (ctype &&& ref) $ reverse $ x : groupKeys
    commas  = Exp . intercalate "," . map getExp
    handler = case observeType of
      Every   -> \msg next -> if msg `Set.member` (_deps deps)
          then do
            next <- next msg
            (fmts, exps) <- unzip <$> mapM (uncurry fmt) toPrint
            return [iTrim|
              printf("${commas fmts}\\n", ${commas exps});
              ${next}
              |]
          else next msg
      Summary -> hempty
    cleanup = case observeType of
      Summary -> \msg next -> runOnce msg (next msg) $ do
        (fmts, exps) <- unzip <$> mapM (uncurry fmt) toPrint
        next <- next msg
        let gs = reverse $ zip (x : groupKeys) groups
        return $ forLoop fmts exps gs ++ next
      Every   -> hempty
      where
        -- only generate code for one message (or once if there are no messages)
        runOnce msg done code = if maybe True (msg ==) (maybeHead msgs)
          then code
          else done
        msgs = _outgoingMessages (_proto spec)
        forLoop fmts exps gs = go gs
          where
            gid gs = Exp $ intercalate "->" $ getIdentifier . fst . snd <$> gs
            go []     = error "Programmer error: Recursed too far"
            go [x]    = [i|printf("${commas fmts}\\n", ${commas exps});|]
            go (g:gs) = let
              kref = ref (fst g)       -- state->key
              g'   = case maybeHead gs of
                Nothing -> error "Invariant violated: list < len 1"
                Just g  -> g
              gref = ref (snd (snd g')) -- state->group
              in [iTrim|for (auto it = ${gid (g:gs)}.begin(); it != ${gid (g:gs)}.end(); ++it)
                {
                  ${kref} = it->first;
                  ${gref} = &(it->second);
                  ${go gs}
                }
              |]

  return $ CompInfo (src x) (ref x) (ctype x) deps tmp cleanup handler

assignNodeIds :: Ord a
  => Signal a
  -> (NodeID, Nodes a (Context IGroup))
assignNodeIds ast = (,) root (IMap.fromList . map swap . Map.toList $ st)
  where
    (root, st) = runState (go ast) mempty
    runGroup (Context (Group g) a) = do
      g <- mapM go g
      return (Context g a)
    go (Fix ast) = do
      -- assign node ids for all the group signals
      ast <- mapM go ast
      ast <- mapCtxM runGroup ast
      st  <- get
      case Map.lookup ast st of
        Just nid -> return nid
        Nothing  -> do
          let nid = Map.size st
          put (Map.insert ast nid st)
          return nid

mapCtx :: (ctx -> ctx') -> ASTF a ctx b -> (ASTF a ctx' b)
mapCtx f = runIdentity . mapCtxM (return . f)

mapCtxM :: Monad m => (ctx -> m ctx') -> ASTF a ctx b -> m (ASTF a ctx' b)
mapCtxM f ast = case ast of
  ZipWith ctx op x y  -> run ctx $ \ctx -> ZipWith ctx op x y
  MergeExp ctx x y    -> run ctx $ \ctx -> MergeExp ctx x y
  MapExp ctx f x      -> run ctx $ \ctx -> MapExp ctx f x
  FoldExp ctx op x    -> run ctx $ \ctx -> FoldExp ctx op x
  ProjectExp ctx p    -> run ctx $ \ctx -> ProjectExp ctx p
  GuardExp ctx pred x -> run ctx $ \ctx -> GuardExp ctx pred x
  LastExp ctx x       -> run ctx $ \ctx -> LastExp ctx x
  ObserveExp ctx o x  -> run ctx $ \ctx -> ObserveExp ctx o x
  ConstExp x          -> return (ConstExp x)
  where
    run ctx withCtx = do { ctx <- f ctx; return (withCtx ctx) }

data Dependencies p = Deps
  { _deps      :: Set (Message p)
  , _revdeps   :: Set (Message p) -- perhaps a better name for this is scope
  , _mustStore :: Bool
  }

dempty :: Dependencies p
dempty = Deps Set.empty Set.empty False

-- calculate dependencies
calcDeps :: Constraints a
  => NodeID
  -> Nodes a (Context IGroup)
  -> Nodes a (IContext a)
calcDeps root nodes = IMap.mapWithKey (\k -> mapCtx ((st !. k),)) nodes
  where
    m !. k = case IMap.lookup k m of
      Nothing -> dempty
      Just t  -> t
    st = execState (go root) mempty
    depends nid cnid = do
      go cnid
      -- nid depends on cnid
      cdeps <- maybe dempty id . IMap.lookup cnid <$> get
      modify $ IMap.insertWith
        (\new old -> old { _deps = _deps old <> _deps new })
        nid
        (Deps (_deps cdeps) mempty False)
      -- cnid reverse depends on nid
      deps <- maybe dempty id . IMap.lookup nid <$> get
      modify $ IMap.adjust
        (\c -> c { _revdeps = _revdeps c <> _deps deps }) cnid

    go nid = mapCtxM (mapM go . nodeGroup) ast >> case ast of
      ZipWith _ _ x y -> do
        nid `depends` x
        nid `depends` y
      MergeExp _ x y -> do
        nid `depends` x
        nid `depends` y
      MapExp _ _ x -> do
        nid `depends` x
      FoldExp _ _ x -> do
        nid `depends` x
      ProjectExp _ p -> do
        let (_field, pmsg) = resolveProjection p
        modify $ IMap.insert nid (Deps (Set.singleton pmsg) mempty False)
      GuardExp _ _pred x -> do
        nid `depends` x -- TODO does `nid` reverse depend on pred .. ?
      LastExp _ x -> do
        nid `depends` x
      ObserveExp _ Every x -> do
        nid `depends` x
      ObserveExp _ Summary x -> do
        nid `depends` x
        modify $ IMap.adjust (\c -> c { _mustStore = True }) x
      ConstExp {} -> return ()
      where
        ast = nodes ! nid

compileAST :: Constraints a
  => NodeID
  -> PeregrineC a (CompInfo a)
compileAST root = go root -- could this use hyloM?
  where
    deref nid = do
      nodes <- nodeContext <$> ask
      case IMap.lookup nid nodes of
        Nothing -> error "Nonexistent NodeID!"
        Just t  -> return t
    go nid = (deref nid >>=) $ \ast -> case ast of
      ZipWith ctx op x y  -> compileOnce nid ctx $ do
        x <- go x
        y <- go y
        compileZipWith ctx op x y
      MergeExp ctx x y    -> compileOnce nid ctx $ do
        x <- go x
        y <- go y
        compileMerge ctx x y
      MapExp ctx f x      -> compileOnce nid ctx $ do
        x <- go x
        compileMap ctx f x
      FoldExp ctx op x    -> compileOnce nid ctx $ do
        x <- go x
        compileFold ctx op x
      ProjectExp ctx p    -> compileOnce nid ctx $ do
        compileProjection ctx p
      LastExp ctx x       -> compileOnce nid ctx $ do
        x <- go x
        compileLast ctx x
      GuardExp ctx pred x -> compileOnce nid ctx $ do
        pred <- go pred
        x    <- go x
        compileGuard ctx pred x
      ObserveExp ctx o x  -> compileOnce nid ctx $ do
        x <- go x
        compileObserve ctx o x
      ConstExp c -> return (compileConstant c)

    -- Only compile the node if it hasn't been compiled yet.
    -- It is IMPORTANT to only compile it once otherwise the topsort
    -- won't work.
    compileOnce nid (d, Context group _) action = do
      mexists <- (IMap.lookup nid . nodeInfo) <$> get
      case mexists of
        Just compInfo -> return compInfo

        Nothing       -> do
          compInfo <- action

          appendTopsort compInfo

          -- mark seen
          modify $ \st -> st
            { nodeInfo = IMap.insert nid compInfo (nodeInfo st) }

          -- insert its declaration into its container if it needs to be stored
          let
            structData = (ctype compInfo, src compInfo)
            set Nothing = error "Didn't find group info .. "
            set (Just (i, compInfo, elems)) = Just
              (i, compInfo, structData : elems)

          when (not (tmp compInfo) || _mustStore d) $ modify $ \st -> st
            { groups = Map.alter set group (groups st) }

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
  px <- taqTradePrice
  sz <- taqTradeSize
  value  <- px *. sz
  volume <- foldP Add sz
  value /. volume

sumP :: Signal a -> Peregrine a
sumP xs = foldP Add xs

midpointP :: Signal TAQ -> Peregrine TAQ
midpointP group = groupBy group $ do
  x   <- taqBidPrice + taqAskPrice
  y   <- x /. 2
  return y                               @! "midpoint"

weightedMidpointP :: Signal TAQ -> Peregrine TAQ
weightedMidpointP group = groupBy group $ do
  bidpx  <- taqBidPrice                     @! "bidpx" -- test CSE working
  bidsz  <- taqBidSize
  askpx  <- taqAskPrice
  asksz  <- taqAskSize
  bidval <- bidpx  *. bidsz                 @! "bidval"
  askval <- askpx  *. asksz                 @! "askval"
  totsz  <- bidsz  +. asksz                 @! "totsz"
  tmpsum <- bidval +. askval                @! "tmp"
  pred   <- totsz >. 0
  ret    <- tmpsum /. totsz
  guardP pred ret                           @! "weighted midpoint"

midpointSkew :: Signal TAQ -> Peregrine TAQ
midpointSkew group = do
  normalMid <- midpointP group         @! "midpoint"
  weightMid <- weightedMidpointP group @! "weighted midpoint"
  groupBy group $ do
    weightMid -. normalMid             @! "midpoint skew"

-- Nonce program to use all the AST types
simpleProgram :: Signal TAQ -> Peregrine TAQ
simpleProgram group = do
  midp   <- midpointP group
  symbol <- symbolP
  groupBy symbol $ do
    bid  <- taqBidPrice
    lbid <- lastP bid                       @! "lastbid"
    sum  <- sumP lbid                       -- @! "sumbid"
    pred <- sum >. 0                        @! "pred"
    twice_sum <- sum +. sum                 @! "twice sum"
    x <- guardP pred sum                    @! "guarded sum"
    taqTradePrice * (x /. midp)             @! "weird quantity"

runPeregrine :: Ord a => Peregrine a -> Signal a
runPeregrine peregrine = runReader peregrine (Group [])

runIntermediate :: Constraints a
  => CompEnv a
  -> PeregrineC a b
  -> C (b, CompState a)
runIntermediate env p = runReaderT
  (runStateT p (CompState mempty mempty mempty))
  env

msgHandler :: Constraints a => Specification a -> Peregrine a -> C (CP.MsgHandler a)
msgHandler spec peregrine = do

  let
    ast   = runPeregrine peregrine
    (root, nodeIds) = assignNodeIds ast
    nodes = calcDeps root nodeIds
    env   = CompEnv spec nodes
  iast <- runIntermediate env (compileAST root)

  let
    (CompState nodeInfo struct nodes) = snd iast
    mkCsdecl (ty, id) = CP.mkCsdecl (getIdentifier id) ty

  forM (reverse (Map.elems struct)) $ \(_name, compInfo, elems) -> do
    -- TODO sort elems by C width
    cstruct (getIdentifier (src compInfo)) $ mkCsdecl `map` elems

  foo <- CU.genId "foo"
  topDecl [cdecl|$ty:topStateTy $id:foo;|]
  topDecl [cdecl|$ty:topStateTy *$id:topState = &$id:foo;|]
  let
    handle         = runHandleCont (handler `map` toList nodes)
    cleanupHandler = runHandleCont (cleanup `map` toList nodes)
    empty   = const (pure [])
  return $ CP.MsgHandler handle empty cleanupHandler

-- Turns a peregrine program into compilable code
p2c :: Peregrine TAQ -> C C.Func
p2c prog = cmain taqCSpec =<< msgHandler taqCSpec prog

-- This program represents the sharing problem because multiple nodes refer to
-- the `bid` node and so it will get compiled twice unless we modulo
-- the sharing
problem :: Peregrine TAQ
problem = do
  bid <- taqBidPrice
  y <- bid +. bid
  bid +. y

zipBug :: Peregrine TAQ
zipBug = do
  s <- project taq "quote" "Symbol"
  groupBy s $ do
    bid <- taqBidPrice
    ask <- taqAskPrice
    x   <- bid +. ask                      @! "x"
    y   <- bid -. ask                      @! "y"
    x +. y                                 @! "bug"

tradeField :: String -> Peregrine TAQ -- helper func
tradeField field = project taq "trade" field @! field

taqTradePrice :: Peregrine TAQ
taqTradePrice = tradeField "Price"

taqTradeSize :: Peregrine TAQ
taqTradeSize = tradeField "Shares"

quoteField :: String -> Peregrine TAQ -- helper func
quoteField field = project taq "quote" field @! field

taqBidPrice :: Peregrine TAQ
taqBidPrice = quoteField "Bid Price"

taqBidSize :: Peregrine TAQ
taqBidSize = quoteField "Bid Size"

taqAskPrice :: Peregrine TAQ
taqAskPrice = quoteField "Ask Price"

taqAskSize :: Peregrine TAQ
taqAskSize = quoteField "Ask Size"

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

-- TODO this generates horrible code.
countP :: Signal a -> Peregrine a
countP sig = do
  sumP =<< (sig -. sig) + 1   @! "count"

sqrtP :: Signal a -> Peregrine a
sqrtP = mapP (Math "sqrt")

absP :: Signal a -> Peregrine a
absP = mapP (Math "abs")

infixl 4 <%>
(<%>) :: Monad m => (a -> m b) -> m a -> m b
f <%> mx = join (f <$> mx)

covariance :: Signal a -> Signal a -> Peregrine a
covariance x y = (@! "covariance") $ do
  cross <- (sumP =<< (x *. y))                @! "cross"
  sumx  <- sumP x                             @! "sumx"
  sumy  <- sumP y                             @! "sumy"
  len   <- countP cross                       @! "rawlen"
  pred  <- len >. 1
  len   <- guardP pred len                    @! "len"
  (pure cross - (pure sumx * pure sumy / pure len)) / (pure len - 1)

correlation :: Signal a -> Signal a -> Peregrine a
correlation x y = (@! "correlation") $ do
  cross <- covariance x y
  sdx   <- sqrtP =<< covariance x x
  sdy   <- sqrtP =<< covariance y y
  pure cross / (pure sdx * pure sdy)

opts :: CP.CompileOptions
opts = CP.CompileOptions
  { debug    = True
  , optLevel = 3
  , compiler = GCC
  , filename = "peregrine"
  }

type Pool a = Chan a

withPool :: Pool a -> (a -> IO b) -> IO b
withPool q io = bracket (readChan q) (writeChan q) io

mkPool :: [a] -> IO (Pool a)
mkPool as = do
  pool <- newChan
  writeList2Chan pool as
  return pool

main = do
  CP.compile opts "bin" . p2c $ do
    s <- symbolP
    mp <- midpointSkew s
    -- ret <- vwapP s
    groupBy s $ do
      ret <- join $ correlation <$> pure mp <*> taqTradePrice
      summary ret

  pool <- mkPool (take 8{-num core-} (repeat ()))
  files <- listDirectory "data/TAQ/"
  timer "total" $ forConcurrently_ files $ \file -> withPool pool $ \() -> do
    path <- pure $ "data/TAQ/" ++ file
    timer file $ callCommand [i|bin/peregrine < ${path}|]

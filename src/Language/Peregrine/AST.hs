{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Language.Peregrine.AST where

import           Data.Fix
import           Protocol.Backend.C.Base as CP
import           Protocol
import qualified Language.C.Utils as C
import           Control.Monad.Identity (runIdentity)

data Constant
  = ConstInt Int
  | ConstDouble Double
  | ConstBool Bool
  deriving (Show, Eq, Ord)

data Observation = Every | Summary
  deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div | Sub | Gt | Ge | Lt | Le | Eq | Max | Min
  deriving (Eq, Ord, Show)
 
data UnaryOp
  = Math String -- function name from cmath e.g. "abs"
  | Cast C.Type
  | Window String -- member name on function
  deriving (Eq, Ord, Show)

data Projection = Projection
  { _pproto     :: Proto CField
  , _pmsgName   :: String
  , _pfieldName :: String
  } deriving (Eq, Ord)
 
instance Show Projection where
  show (Projection p msg field) = (_namespace p)
    ++ "\".\""
    ++ msg
    ++ "\".\""
    ++ field
    ++ "\""

data ASTF
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
  | ProjectExp ctx Projection
  -- | A rolling window
  --   TODO generalize the window size to arbitrary predicate
  | WindowExp ctx BinOp Int {-win size in millis-} next {-timestamp-} next {-x-}
  -- | Only send updates downstream if the argument is satisfied
  | GuardExp ctx next next
  -- | One before current
  | LastExp ctx next
  -- | Restrict x to only fire when y fires
  | RestrictExp ctx next next
  -- | Observe an expression
  | ObserveExp ctx Observation next
  -- | A constant
  | ConstExp Constant
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
 
-- front-end peregrine context
data Context g = Context
  { nodeGroup  :: g
  , annotation :: Maybe String
  } deriving (Show, Functor, Foldable, Traversable)
 
-- Special Eq and Ord instances for Context
-- because we don't want the annotation or generated ids
-- to have any effect on map lookups
instance Eq a => Eq (Context a) where
  Context group1 _ == Context group2 _ = group1 == group2
instance Ord a => Ord (Context a) where
  Context group1 _ `compare` Context group2 _ = group1 `compare` group2

mapCtx :: (ctx -> ctx') -> ASTF ctx b -> (ASTF ctx' b)
mapCtx f = runIdentity . mapCtxM (return . f)
  
mapCtxM :: Monad m => (ctx -> m ctx') -> ASTF ctx b -> m (ASTF ctx' b)
mapCtxM f ast = case ast of
  ZipWith ctx op x y    -> run ctx $ \ctx -> ZipWith ctx op x y
  MergeExp ctx x y      -> run ctx $ \ctx -> MergeExp ctx x y
  MapExp ctx f x        -> run ctx $ \ctx -> MapExp ctx f x
  FoldExp ctx op x      -> run ctx $ \ctx -> FoldExp ctx op x
  ProjectExp ctx p      -> run ctx $ \ctx -> ProjectExp ctx p
  WindowExp ctx o p x y -> run ctx $ \ctx -> WindowExp ctx o p x y
  GuardExp ctx pred x   -> run ctx $ \ctx -> GuardExp ctx pred x
  LastExp ctx x         -> run ctx $ \ctx -> LastExp ctx x
  RestrictExp ctx x y   -> run ctx $ \ctx -> RestrictExp ctx x y
  ObserveExp ctx o x    -> run ctx $ \ctx -> ObserveExp ctx o x
  ConstExp x            -> return (ConstExp x)
  where   
    run ctx withCtx = do { ctx <- f ctx; return (withCtx ctx) }


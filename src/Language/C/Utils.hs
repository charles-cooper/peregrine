-- Monad for a C EDSL

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.C.Utils (
  C,
  GType(..),
  Includes(..),
  depends,
  include,
  require,
  topDecl,
  noloc,
  mkCompUnit,
  genId,
  Identifiable(..),
  idExp,

  char,
  bool,
  ushort,
  uint,
  ulong,
  int,
  double,

  boolExp,
) where

import Language.Utils

import           Language.C.Quote.C
import qualified Language.C.Syntax as C

import Text.InterpolatedString.Perl6 (qc)

import Data.Loc (SrcLoc(..), Loc(..))

import           Control.Monad.State
import           Data.Monoid
import           Data.Maybe
import           Data.List (nub)
import           Data.Tuple (swap)
import qualified Data.Map as Map
import           Data.Map (Map(..))

char   = [cty|char|]
uchar  = [cty|typename uint8_t|]
bool   = [cty|typename bool|]
ushort = [cty|typename uint16_t|]
uint   = [cty|typename uint32_t|]
ulong  = [cty|typename uint64_t|]
int    = [cty|typename int32_t|]
double = [cty|double|]

data CState = CState
  { _compunit :: CompUnit
  , _ids      :: IDGenerator
  }

-- Bool has no toExp instance
boolExp :: Bool -> C.Exp
boolExp True  = [cexp|($ty:bool)1|]
boolExp False = [cexp|($ty:bool)0|]

-- Create an expression from an identifier
idExp :: String -> C.Exp
idExp id = [cexp|$id:id|]

data CompUnit = CompUnit
  { includes  :: InsertionSet Includes
  , decls     :: InsertionSet C.Definition
  -- ^ Useful for inserting raw C++ declarations
  , typedecls :: InsertionSet C.Type
  , functions :: InsertionSet C.Func
  , topLevel  :: InsertionSet C.InitGroup
  }

instance Monoid CompUnit where
  mempty = CompUnit mempty mempty mempty mempty mempty
  mappend (CompUnit a b c d e) (CompUnit a' b' c' d' e') =
    CompUnit (a<>a') (b<>b') (c<>c') (d<>d') (e<>e')

instance Monoid CState where
  mempty = CState mempty mempty
  mappend (CState a b) (CState a' b') = CState (a<>a') (b<>b')

-- TODO come up with better name
genId :: String -> C String
genId str = do
  st <- get
  let (id, ids') = newId str (_ids st)
  put st { _ids = ids' }
  return id

newtype C a = C { getState :: State CState a }
  deriving (Functor, Applicative, Monad, MonadState CState)

depends :: TopLevel a => a -> C a
depends d = do
  modify (\cstate -> cstate { _compunit = _compunit cstate <>define d } )
  return d

-- another way of saying 'id'
require :: C a -> C a
require = id

-- monomorphic version of depends for top-level declarations
topDecl :: C.InitGroup -> C C.InitGroup
topDecl = depends

include :: String -> C Includes
include = depends . Includes

getCompUnit :: C a -> CompUnit
getCompUnit c = _compunit $ execState (getState c) mempty

newtype Includes = Includes String deriving (Eq, Ord, Show)
class TopLevel a where
  define :: a -> CompUnit
instance TopLevel Includes where
  define inc  = CompUnit (single inc) e e e e
instance TopLevel C.Definition where
  define def  = CompUnit e (single def) e e e
instance TopLevel C.Type where
  define ty   = CompUnit e e (single ty) e e
instance TopLevel C.Func where
  define func = CompUnit e e e (single func) e
instance TopLevel C.InitGroup where
  define init = CompUnit e e e e (single init)

e :: Monoid m => m
e = mempty

single :: Ord a => a -> InsertionSet a
single a = push a mempty

noloc :: SrcLoc
noloc = SrcLoc NoLoc

-- General C type. C AST puts Array metadata in a different part.
-- This data type lets us pass around array-objects which we can
-- then compile to C AST
data GType = ArrayTy  { arrayty :: C.Type, arraysize :: Int }
           | SimpleTy { simplety :: C.Type }
  deriving Eq

mkCompUnit :: C a -> [C.Definition]
mkCompUnit code = [cunit|
    /* Includes */
    $edecls:(include <$> fromInsertionSet incls)
 
    /* Types */
    $edecls:(typedecl <$> fromInsertionSet tys)
 
    /* Other definitions */
    $edecls:(fromInsertionSet defs)
 
    /* Top level declarations */
    $edecls:(declare <$> fromInsertionSet decls)
 
    /* Functions */
    $edecls:(fundecl <$> fromInsertionSet funcs)
  |]
  where
    includeStr header          = [qc|#include <{header::String}>|]
    include (Includes header)  = [cedecl|$esc:(includeStr header)|]
    typedecl ty                = [cedecl|$ty:ty;|]
    fundecl func               = [cedecl|$func:func|]
    declare decl               = [cedecl|$decl:decl;|]
    (CompUnit incls defs tys funcs decls) = getCompUnit code

class Identifiable a where
  getId :: a -> C.Id

instance Identifiable C.Func where
  getId (C.Func _ iden _ _ _ _)      = iden
  getId (C.OldFunc _ iden _ _ _ _ _) = iden


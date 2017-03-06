-- Monad for a C EDSL

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

  Type(..),
  Identifier(..),
  Exp(..),
  Func,
  Code,

  cfun,
  cty,
  stms,

  char,
  bool,
  double,

  uchar,
  ushort,
  uint,
  ulong,

  schar,
  short,
  int,
  long,

  isNumeric,
  signed,
  width,
  signedOf,

  boolExp,
) where

import Language.Utils

import Text.InterpolatedString.Perl6 (qc)
import Data.String.Interpolate
import Data.String

import Data.Loc (SrcLoc(..), Loc(..))

import           Control.Monad.State
import           Data.Monoid
import           Data.Maybe
import           Data.List (nub, intercalate)
import           Data.Tuple (swap)
import qualified Data.Map as Map
import           Data.Map (Map(..))

char   = Type [i|char|]
bool   = Type [i|bool|]
double = Type [i|double|]

schar  = Type [i|int8_t|]
short  = Type [i|int16_t|]
int    = Type [i|int32_t|]
long   = Type [i|int64_t|]

uchar  = Type [i|uint8_t|]
ushort = Type [i|uint16_t|]
uint   = Type [i|uint32_t|]
ulong  = Type [i|uint64_t|]

isNumeric :: Type -> Bool
isNumeric ty = ty `elem`
  [ uchar
  , ushort
  , uint
  , ulong
  , schar
  , short
  , int
  , long
  , double
  ]

signed :: Type -> Bool
signed ty = ty `elem` [schar, short, int, long]

width :: Type -> Int
width ty = case () of
  _ | ty `elem` [char, schar, uchar]  -> 1
  _ | ty `elem` [short, ushort]       -> 2
  _ | ty `elem` [int, uint]           -> 4
  _ | ty `elem` [long, ulong, double] -> 8
  _ | otherwise -> error $ "Unknown width for type " <> show ty

-- Convert an unsigned integral type into its signed counterpart
signedOf :: Type -> Type
signedOf ty = if
  | ty == uchar  -> schar
  | ty == ushort -> short
  | ty == uint   -> int
  | ty == ulong  -> long
  | otherwise    -> ty

data CState = CState
  { _compunit :: CompUnit
  , _ids      :: IDGenerator
  }

-- Bool has no toExp instance
boolExp :: Bool -> Exp
boolExp True  = Exp [i|true|]
boolExp False = Exp [i|false|]

newtype Func = Func String
  deriving (Eq, Ord)
newtype InitGroup = InitGroup String
  deriving (Eq, Ord)
newtype Definition = Definition String
  deriving (Eq, Ord)
instance Show Func where show (Func d) = d
instance IsString Func where fromString = Func
instance Show InitGroup where show (InitGroup d) = d
instance IsString InitGroup where fromString = InitGroup
instance Show Definition where show (Definition d) = d
instance IsString Definition where fromString = Definition

data CompUnit = CompUnit
  { includes  :: InsertionSet Includes
  , defs      :: InsertionSet Definition
  , functions :: InsertionSet Func
  , topLevel  :: InsertionSet InitGroup
  }

instance Monoid CompUnit where
  mempty = CompUnit mempty mempty mempty mempty
  mappend (CompUnit a b c d) (CompUnit a' b' c' d') =
    CompUnit (a<>a') (b<>b') (c<>c') (d<>d')

instance Monoid CState where
  mempty = CState mempty mempty
  mappend (CState a b) (CState a' b') = CState (a<>a') (b<>b')

-- TODO come up with better name
genId :: String -> C Identifier
genId str = do
  st <- get
  let (id, ids') = newId str (_ids st)
  put st { _ids = ids' }
  return (Identifier id)

newtype C a = C { getState :: State CState a }
  deriving (Functor, Applicative, Monad, MonadState CState)

depends :: TopLevel a => a -> C a
depends d = do
  modify (\cstate -> cstate { _compunit = _compunit cstate <>define d } )
  return d

cfun :: String -> C Func
cfun = depends . Func

cty :: String -> C Definition
cty = depends . Definition

stms :: [Code] -> Code
stms = intercalate "\n" . map (++";")

-- another way of saying 'id'
require :: C a -> C a
require = id

-- monomorphic version of depends for top-level declarations
topDecl :: InitGroup -> C InitGroup
topDecl = depends

include :: String -> C Includes
include = depends . Includes

getCompUnit :: C a -> CompUnit
getCompUnit c = _compunit $ execState (getState c) mempty

newtype Includes = Includes String deriving (Eq, Ord, Show)
class TopLevel a where
  define :: a -> CompUnit
instance TopLevel Includes where
  define inc  = CompUnit (single inc) e e e
instance TopLevel Definition where
  define def  = CompUnit e (single def) e e
instance TopLevel Func where
  define func = CompUnit e e (single func) e
instance TopLevel InitGroup where
  define init = CompUnit e e e (single init)

e :: Monoid m => m
e = mempty

single :: Ord a => a -> InsertionSet a
single a = push a mempty

noloc :: SrcLoc
noloc = SrcLoc NoLoc

newtype Type = Type { getType :: String }
  deriving (Eq, Ord)

-- General C type. C AST puts Array metadata in a different part.
-- This data type lets us pass around array-objects which we can
-- then compile to C AST
data GType = ArrayTy  { arrayty :: Type, arraysize :: Int }
           | SimpleTy { simplety :: Type }
  deriving Eq

type Code = String

mkCompUnit :: C a -> Code
mkCompUnit code = [i|
    /* Includes */
    ${intercalate "\n" $ include <$> fromInsertionSet incls}
 
    /* Types */
    ${intercalate "\n" $ define <$> fromInsertionSet defs}
 
    /* Top level declarations */
    ${declare `concatMap` fromInsertionSet decls}
 
    /* Functions */
    ${fundecl `concatMap` fromInsertionSet funcs}
  |]
  where
    includeStr header          = [i|#include <${header}>|]
    include (Includes header)  = [i|${includeStr header}|]
    fundecl func               = [i|${func}|]
    define (Definition def)    = [i|${def};|]
    declare decl               = [i|${decl};|]
    (CompUnit incls defs funcs decls) = getCompUnit code

newtype Exp = Exp { getExp :: String }
  deriving (Eq, Ord, Monoid)

instance Show Exp where show = getExp
instance Show Identifier where show = getIdentifier
instance Show Type where show = getType

instance IsString Exp where fromString = Exp
instance IsString Identifier where fromString = Identifier
instance IsString Type where fromString = Type

newtype Identifier = Identifier { getIdentifier :: String }
  deriving (Eq, Ord, Monoid)


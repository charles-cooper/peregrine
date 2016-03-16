{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.C.Utils (
  C,
  GType(..),
  Includes(..),
  CompUnit(..),
  depends,
  include,
  require,
  topDecl,
  noloc,
  getCompUnit,
  mkCompUnit,
  genId,
  Identifiable(..),

  char,
  bool,
  ushort,
  uint,
  ulong,
) where

import           Language.C.Quote.C
import qualified Language.C.Syntax as C

import Text.InterpolatedString.Perl6 (qc)

import Data.Loc (SrcLoc(..), Loc(..))

import           Control.Monad.State
import           Data.Monoid
import           Data.Maybe
import           Data.List (nub)
import qualified Data.Map as Map
import           Data.Map (Map(..))

char   = [cty|char|]
uchar  = [cty|typename uint8_t|]
bool   = [cty|typename bool|]
ushort = [cty|typename uint16_t|]
uint   = [cty|typename uint32_t|]
ulong  = [cty|typename uint64_t|]

data CState = CState
  { _compunit :: CompUnit
  , _ids      :: Map String Int
  }

data CompUnit = CompUnit
  { includes  :: [Includes]
  , decls     :: [C.Definition] -- useful for inserting raw C++ declarations
  , typedecls :: [C.Type]
  , functions :: [C.Func]
  , topLevel  :: [C.InitGroup]
  }

instance Monoid CompUnit where
  mempty = CompUnit [] [] [] [] []
  mappend (CompUnit a b c d e) (CompUnit a' b' c' d' e') =
    CompUnit (a<>a') (b<>b') (c<>c') (d<>d') (e<>e')

instance Monoid CState where
  mempty = CState mempty mempty
  mappend (CState a b) (CState a' b') = CState (a<>a') (b<>b')

genId :: String -> C String
genId str = do
  st <- get
  let (id, newids) = count str $ _ids $ st
  put $ st { _ids = newids }
  return $ str ++ "_" ++ show id
  where
    count a old = let
      f _k new old = new + old
      (mret, new) = Map.insertLookupWithKey f a 1 old
      in (fromMaybe 0 mret, new)


type C a = State CState a
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
getCompUnit c = _compunit $ execState c mempty

newtype Includes = Includes String deriving (Eq, Ord, Show)
class TopLevel a where
  define :: a -> CompUnit
instance TopLevel Includes where
  define inc  = CompUnit [inc] [] [] [] []
instance TopLevel C.Definition where
  define def  = CompUnit [] [def] [] [] []
instance TopLevel C.Type where
  define ty   = CompUnit [] [] [ty] [] []
instance TopLevel C.Func where
  define func = CompUnit [] [] [] [func] []
instance TopLevel C.InitGroup where
  define init = CompUnit [] [] [] [] [init]

noloc :: SrcLoc
noloc = SrcLoc NoLoc

-- General C type. C AST puts Array metadata in a different part.
-- This data type lets us pass around array-objects which we can
-- then compile to C AST
data GType = ArrayTy  { arrayty :: C.Type, arraysize :: Int }
           | SimpleTy { simplety :: C.Type }

mkCompUnit :: C a -> [C.Definition]
mkCompUnit code = [cunit|
    /* Includes */
    $edecls:(nub $ include <$> incls)
 
    /* Types */
    $edecls:(typedecl <$> nub tys)
 
    /* Other definitions */
    $edecls:(nub $ defs)
 
    /* Top level declarations */
    $edecls:(declare <$> nub decls)
 
    /* Functions */
    $edecls:(fundecl <$> nub funcs)
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


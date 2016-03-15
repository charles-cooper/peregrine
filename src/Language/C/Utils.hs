module Language.C.Utils (
  C,
  GType(..),
  Includes(..),
  CompUnit(..),
  depends,
  noloc,
  getCompUnit
) where

import qualified Language.C.Syntax as C

import Data.Loc (SrcLoc(..), Loc(..))

import Control.Monad.State
import Data.Monoid

data CompUnit = CompUnit
  { includes  :: [Includes]
  , typedecls :: [C.Type]
  , functions :: [C.Func]
  }

instance Monoid CompUnit where
  mempty = CompUnit [] [] []
  mappend (CompUnit a b c) (CompUnit a' b' c') =
    CompUnit (a<>a') (b<>b') (c<>c')

type C a = State CompUnit a
depends :: TopLevel a => a -> C CompUnit
depends d = do
  modify (<>cu)
  return $ cu
  where
    cu = define d

getCompUnit :: C a -> CompUnit
getCompUnit c = execState c mempty

newtype Includes = Includes String
class TopLevel a where
  define :: a -> CompUnit
instance TopLevel Includes where
  define inc  = CompUnit [inc] [] []
instance TopLevel C.Type where
  define ty   = CompUnit [] [ty] []
instance TopLevel C.Func where
  define func = CompUnit [] [] [func]

noloc :: SrcLoc
noloc = SrcLoc NoLoc

-- General C type. C AST puts Array metadata in a different part.
-- This data type lets us pass around array-objects which we can
-- then compile to C AST
data GType = ArrayTy  { arrayty :: C.Type, arraysize :: Int }
           | SimpleTy { simplety :: C.Type }


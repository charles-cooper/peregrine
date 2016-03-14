{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Protocol.Tmx.TAQ as TAQ
import Protocol

import Language.C.Quote.C
import Language.C.Smart ()
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland (putDocLn, ppr, pretty)
import Text.InterpolatedString.Perl6 (qc)
import Data.Loc (SrcLoc(..), Loc(..))

import Data.List (nub, sort)
import Data.Monoid

import Control.Lens
import Control.Monad (forM)

import System.Directory
import System.Process
import System.Exit
import System.IO

noloc = SrcLoc NoLoc

char   = [cty|char|]
uchar  = [cty|typedef unsigned char uint8_t|]
bool   = [cty|typename bool|]
ushort = [cty|typename uint16_t|]
uint   = [cty|typename uint32_t|]
ulong  = [cty|typename uint64_t|]

assert :: Bool -> a -> a
assert pred a = if pred then a else (error "Assertion failed")

genStruct :: Message TAQ -> CompUnit -- C.Type
genStruct msg = CompUnit [] [ty] []
  where
    ty = [cty|
      struct $id:(msg^.msgName) {
        $sdecls:decls
      }
      |]
    decls = mkDecl <$> (msg^.fields)
    mkDecl f@(Field _ len nm ty notes)
      | ty==Numeric    && len==9 = [csdecl|$ty:uint   $id:cnm;|]
      | ty==Numeric    && len==7 = [csdecl|$ty:uint   $id:cnm;|]
      | ty==Numeric    && len==3 = [csdecl|$ty:ushort $id:cnm;|]
      | ty==Numeric              = error $ "Unknown integer len for " ++ show f
      | ty==Alphabetic && len==1 = [csdecl|$ty:char   $id:cnm;|]
      | ty==Alphabetic           = [csdecl|$ty:char   $id:cnm[$const:len];|]
      | ty==Boolean              = assert (len==1) [csdecl|$ty:bool $id:cnm;|]
      | ty==Date                 = [csdecl|$ty:uint   $id:cnm;|]
      | ty==Time                 = [csdecl|$ty:uint   $id:cnm;|]
      where
        cnm = rawIden (cname nm)

readStruct :: Message TAQ -> CompUnit
readStruct msg = CompUnit [] [] $ pure [cfun|
    void $id:(funName) (struct $id:(structName) *dst, char const *buf) {
      /* TODO */
      return ;
    }
  |]
  where
    funName :: String = [qc|read_{msg^.msgName}|]
    structName        = msg^.msgName

data CompUnit = CompUnit
  { includes  :: [String]
  , typedecls :: [C.Type]
  , functions :: [C.Func]
  }
instance Monoid CompUnit where
  mempty = CompUnit [] [] []
  mappend (CompUnit a b c) (CompUnit a' b' c') =
    CompUnit (a<>a') (b<>b') (c<>c')

mkCompUnit :: CompUnit -> [C.Definition]
mkCompUnit (CompUnit includes types funcs) = [cunit|
    /* Includes */
    $edecls:(nub $ sort $ include <$> includes)

    /* Types */
    $edecls:(typedecl <$> types)

    /* Functions */
    $edecls:(fundecl <$> funcs)
  |]
  where
    include (header :: String) = C.EscDef [qc|#include <{header}>|] noloc
    typedecl ty                = C.DecDef [cdecl|$ty:ty;|] noloc
    fundecl func               = C.FuncDef func noloc

cReadIntegral ty = CompUnit ["cstdint", "cassert", "cctype"] [] $ pure [cfun|
  $ty:ty $id:(funName) (char const *buf, $ty:uint len) {
    $ty:ty ret;
    while (len--) {
      assert(isdigit(*buf));
      ret = ret * 10 + (*buf - '0');
      ++buf;
    }
    return ret;
  }
  |]
  where
    funName :: String = [qc|parse_{pretty 0 $ ppr ty}|]

cmain = CompUnit [] [] $ pure [cfun|int main() {} |]

----------------------------
-- Compilation
----------------------------

getBuildDir :: IO FilePath
getBuildDir = (++"/bin") <$> getCurrentDirectory

compile :: CompUnit -> IO ()
compile code = do
  buildDir <- getBuildDir
  createDirectoryIfMissing False buildDir
  let intermediateFile = [qc|{buildDir}/main.cpp|]
  writeFile intermediateFile $ pretty 80 (ppr (mkCompUnit code))

  (_, _, _, gcc) <- createProcess $
    (shell [qc|g++ -std=c++11 -O2 -g {intermediateFile}|])
    { cwd = Just buildDir }

  ecode <- waitForProcess gcc
  case ecode of
    ExitSuccess   -> return ()
    ExitFailure _ -> error "gcc failed."

main = do
  compile $ cReadIntegral uint
    <> (mconcat $ genStruct <$> taq^.outgoingMessages)
    <> (mconcat $ readStruct <$> taq^.outgoingMessages)
    <> cmain

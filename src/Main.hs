{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Protocol.Tmx.TAQ as TAQ
import Protocol

import           Language.C.Quote.C
import           Language.C.Smart ()
import qualified Language.C.Syntax as C
import qualified Language.C.Utils as C
import           Language.C.Utils (C, CompUnit, depends, noloc)

import Text.PrettyPrint.Mainland (putDocLn, ppr, pretty, prettyPragma)
import Text.InterpolatedString.Perl6 (qc)

import Data.List (nub, sort)

import Control.Lens
import Control.Monad

import System.Directory
import System.Process
import System.Exit
import System.IO

import Development.Shake
import Development.Shake.FilePath

char   = [cty|char|]
uchar  = [cty|typename uint8_t|]
bool   = [cty|typename bool|]
ushort = [cty|typename uint16_t|]
uint   = [cty|typename uint32_t|]
ulong  = [cty|typename uint64_t|]

assert :: Bool -> a -> a
assert pred a = if pred then a else (error "Assertion failed")

mkCsdecl :: String -> C.GType -> C.FieldGroup
mkCsdecl nm (C.SimpleTy ty)   = [csdecl|$ty:ty $id:nm;|]
mkCsdecl nm (C.ArrayTy ty sz) = [csdecl|$ty:ty $id:nm[$const:sz]; |]

genStruct :: Message TAQ -> C.Type
genStruct msg = ty
  where
    ty = [cty|
      struct $id:(msg^.msgName) {
        $sdecls:decls
      }
    |]
    decls = declare <$> (msg^.fields)
    declare f@(Field _ len nm ty _) = mkCsdecl (rawIden $ cname nm) $ mkTy f

mkTy :: Field TAQ -> C.GType
mkTy f@(Field _ len _ ty _)
  | ty==Numeric    && len==9 = C.SimpleTy uint
  | ty==Numeric    && len==7 = C.SimpleTy uint
  | ty==Numeric    && len==3 = C.SimpleTy ushort
  | ty==Numeric              = error $ "Unknown integer len for " ++ show f
  | ty==Alphabetic && len==1 = C.SimpleTy char
  | ty==Alphabetic           = C.ArrayTy char len
  | ty==Boolean              = assert (len==1) $ C.SimpleTy bool
  | ty==Date                 = C.SimpleTy uint -- assert?
  | ty==Time                 = C.SimpleTy uint -- assert?
  | otherwise                = error "Unknown case."

readStruct :: Message TAQ -> C CompUnit
readStruct msg = do
  depends $ genStruct msg
  depends $ C.Includes "cstring"
  depends =<< impl
  where
    impl = do
      pureStms <- stms
      return [cfun|
        void $id:(funName) (struct $id:(structName) *dst, char const *buf) {
          $stms:pureStms
        }
      |]
    funName :: String = [qc|read_{msg^.msgName}|]
    structName        = msg^.msgName
    ofsts             = scanl (+) 0 (msg^.fields & map _len)

    -- don't have a good way of grabbing all called functions.
    -- this is a crude way of stitching them in by hand.
    readMemberDecl f@(Field _ len _ ty _) = case ty of
      Numeric -> Just <$> cReadIntegral (C.simplety $ mkTy f)
      _ -> return Nothing

    readMember ofst f@(Field _ len nm ty _) = case ty of

      Numeric    -> runIntegral

      Alphabetic -> return $ if len == 1
                      then [cstm|dst->$id:cnm = *buf;|]
                      else [cstm|memcpy(&dst->$id:cnm, buf, $const:len);|]

      Boolean    -> return [cstm|dst->$id:cnm = (*buf == '1');|]

      Date       -> runIntegral
      Time       -> runIntegral
      where
        runIntegral = do
          dep <- cReadIntegral (C.simplety $ mkTy f)
          return [cstm|dst->$id:cnm = $id:(getId dep) (buf, $const:len); |]

        cnm     = rawIden (cname nm)

    stms = zipWithM readMember ofsts (msg^.fields)

class Identifiable a where
  getId :: a -> C.Id

instance Identifiable C.Func where
  getId (C.Func _ iden _ _ _ _)      = iden
  getId (C.OldFunc _ iden _ _ _ _ _) = iden

mkCompUnit :: C a -> [C.Definition]
mkCompUnit code = [cunit|
    /* Includes */
    $edecls:(nub $ sort $ include <$> incls)

    /* Types */
    $edecls:(typedecl <$> nub tys)

    /* Functions */
    $edecls:(fundecl <$> nub funcs)
  |]
  where
    include (C.Includes header)  = C.EscDef [qc|#include <{header}>|] noloc
    typedecl ty                  = C.DecDef [cdecl|$ty:ty;|] noloc
    fundecl func                 = C.FuncDef func noloc
    (C.CompUnit incls tys funcs) = C.getCompUnit code

cReadIntegral ty = do
  mapM (depends . C.Includes) ["cstdint", "cassert", "cctype"]
  depends impl
  return impl
  where
    funName :: String = [qc|parse_{pretty 0 $ ppr ty}|]
    impl = [cfun|
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

cmain = depends [cfun|int main() {} |]

----------------------------
-- Compilation
----------------------------

compile :: C a -> Rules ()
compile code = do

  let buildDir = "bin"
  let src = [qc|{buildDir}/main.cpp|]
  let out = [qc|{buildDir}/a.out|]

  out %> \out -> do
    need [src]
    command_ [Cwd buildDir, Shell] [qc|g++ -std=c++11 -O2 -g main.cpp|] []

  src %> \out -> do
    writeFile' out (prettyPragma 80 (ppr (mkCompUnit code)))

  want [out]

main = do
  shakeArgs shakeOptions $ do
    compile $ do
      mapM readStruct $ taq^.outgoingMessages
      cmain
    want ["data/20150826TradesAndQuotesDaily.lz4"]
    "data/*.lz4" %> \out -> do
      let src = "/mnt/efs/ftp/tmxdatalinx.com" </> (takeFileName out) -<.> "gz"
      need [src]
      command [Shell] [qc|gzip -d < {src} | lz4 -9 > {out}|] []


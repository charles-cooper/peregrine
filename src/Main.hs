{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Protocol.Tmx.TAQ as TAQ
import Protocol

import Language.C.Quote.C
import Language.C.Smart ()
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland (putDocLn, ppr, pretty, prettyPragma)
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
uchar  = [cty|typename uint8_t|]
bool   = [cty|typename bool|]
ushort = [cty|typename uint16_t|]
uint   = [cty|typename uint32_t|]
ulong  = [cty|typename uint64_t|]

assert :: Bool -> a -> a
assert pred a = if pred then a else (error "Assertion failed")

-- TODO put this in an 'import as C' module
data CGenTy = ArrayTy  { arrayty :: C.Type, arraysize :: Int }
            | SimpleTy { simplety :: C.Type }

mkCsdecl :: String -> CGenTy -> C.FieldGroup
mkCsdecl nm (SimpleTy ty)   = [csdecl|$ty:ty $id:nm;|]
mkCsdecl nm (ArrayTy ty sz) = [csdecl|$ty:ty $id:nm[$const:sz]; |]

genStruct :: Message TAQ -> CompUnit
genStruct msg = CompUnit [] [ty] []
  where
    ty = [cty|
      struct $id:(msg^.msgName) {
        $sdecls:decls
      }
      |]
    decls = declare <$> (msg^.fields)
    declare f@(Field _ len nm ty _) = mkCsdecl (rawIden $ cname nm) $ mkTy f

mkTy :: Field TAQ -> CGenTy
mkTy f@(Field _ len _ ty _)
  | ty==Numeric    && len==9 = SimpleTy uint
  | ty==Numeric    && len==7 = SimpleTy uint
  | ty==Numeric    && len==3 = SimpleTy ushort
  | ty==Numeric              = error $ "Unknown integer len for " ++ show f
  | ty==Alphabetic && len==1 = SimpleTy char
  | ty==Alphabetic           = ArrayTy char len
  | ty==Boolean              = assert (len==1) $ SimpleTy bool
  | ty==Date                 = SimpleTy uint -- assert?
  | ty==Time                 = SimpleTy uint -- assert?
  | otherwise                = error "Unknown case."

readStruct :: Message TAQ -> CompUnit
readStruct msg = CompUnit includes [] $ (concatMap readMemberDecl (msg^.fields)) ++ [impl]
  where
    includes = ["cstring"]
    impl = [cfun|
      void $id:(funName) (struct $id:(structName) *dst, char const *buf) {
        $stms:stms
      }
    |]
    funName :: String = [qc|read_{msg^.msgName}|]
    structName        = msg^.msgName
    ofsts             = scanl (+) 0 (msg^.fields & map _len)

    -- don't have a good way of grabbing all called functions.
    -- this is a crude way of stitching them in by hand.
    readMemberDecl f@(Field _ len _ ty _) = case ty of
      Numeric -> [cReadIntegralRaw (simplety $ mkTy f)]
      _ -> []

    readMember ofst f@(Field _ len nm ty _) = case ty of

      Numeric    -> [cstm|dst->$id:cnm = $id:funName(buf, $const:len); |]

      Alphabetic -> if len == 1
                      then [cstm|dst->$id:cnm = *buf;|]
                      else [cstm|memcpy(&dst->$id:cnm, buf, $const:len);|]

      Boolean    -> [cstm|dst->$id:cnm = (*buf == '1');|]

      Date       -> [cstm|dst->$id:cnm = $id:funName(buf, $const:len); |]
      Time       -> [cstm|dst->$id:cnm = $id:funName(buf, $const:len); |]
      -- ^ Identical to numeric
      where
        cnm     = rawIden (cname nm)
        funName = getId $ cReadIntegralRaw (simplety $ mkTy f)

    stms = zipWith readMember ofsts (msg^.fields)

class Identifiable a where
  getId :: a -> C.Id

instance Identifiable C.Func where
  getId (C.Func _ iden _ _ _ _)      = iden
  getId (C.OldFunc _ iden _ _ _ _ _) = iden

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
    $edecls:(typedecl <$> nub types)

    /* Functions */
    $edecls:(fundecl <$> nub funcs)
  |]
  where
    include (header :: String) = C.EscDef [qc|#include <{header}>|] noloc
    typedecl ty                = C.DecDef [cdecl|$ty:ty;|] noloc
    fundecl func               = C.FuncDef func noloc

cReadIntegral ty = CompUnit ["cstdint", "cassert", "cctype"]
                            []
                            [cReadIntegralRaw ty]
cReadIntegralRaw ty = [cfun|
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
  writeFile intermediateFile $ prettyPragma 80 (ppr (mkCompUnit code))

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

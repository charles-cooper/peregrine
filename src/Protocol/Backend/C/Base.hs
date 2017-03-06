{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Protocol.Backend.C.Base where

import           Language.C.Utils as C

import           Protocol

import           Development.Shake

import           Text.PrettyPrint.Mainland (putDocLn, ppr, pretty, prettyPragma)
import           Text.InterpolatedString.Perl6
import           Data.String.Interpolate.IsString

import           Data.Bits
import           Data.List (intercalate)

import           Utils
import           Data.Monoid

import           Control.Monad

import           System.IO.Unsafe (unsafePerformIO)
import           System.IO
import           System.Process

data Specification a = Specification
  { _proto      :: Proto a
  , _mkTy       :: Field a -> C C.GType
  , _readMember :: C.Exp -> C.Exp -> Field a -> C Code
  }

data MsgHandler a = MsgHandler
  { _handleMsg  :: Message a -> C Code
  , _initMsg    :: Message a -> C Code
  , _cleanupMsg :: Message a -> C Code
  }

-- non-overlapping monoid instance for (a -> m b)
mempty_ :: (Applicative t, Monoid m) => (a -> t m)
mempty_ _ = pure mempty
mappend_ :: (Applicative t, Monoid m) => (a -> t m) -> (a -> t m) -> (a -> t m)
mappend_ f g = \a -> (<>) <$> f a <*> g a

instance Monoid (MsgHandler a) where
  mempty = MsgHandler empty empty empty
    where empty = const (return mempty)
  mappend (MsgHandler h1 i1 c1) (MsgHandler h2 i2 c2) = MsgHandler h3 i3 c3
    where
      h3 = h1 `mappend_` h2
      i3 = i1 `mappend_` i2
      c3 = c1 `mappend_` c2

cstruct :: Identifier -> [(Type, Identifier)] -> C C.Type
cstruct name members = do
  cty [i|struct ${name} {
    ${body}
  }|]
  return [i|struct ${name}|]
  where
    body = concatMap (\(ty, id) -> [i|${ty} ${id};|]) members

cnm :: String -> Identifier
cnm = Identifier . rawIden . cname

genStruct :: Specification a -> Message a -> C C.Type
genStruct spec msg = do
  decls <- runDecls
  cstruct (cnm $ _msgName msg) decls
  where
    runDecls = mapM declare (_fields msg)
    declare f@(Field _ len nm ty _) = mkCsdecl (rawIden $ cname nm) <$> mkTy f
    mkTy = _mkTy spec

mkCsdecl :: String -> C.GType -> (Type, Identifier)
mkCsdecl nm (C.SimpleTy ty)   = (ty, Identifier nm)
mkCsdecl nm (C.ArrayTy ty sz) = (ty, Identifier [i|${nm}[${sz}]|])
 
readStruct :: Specification a -> Message a -> C Identifier
readStruct spec@(Specification {..}) msg = do
  include "cstring"
  require (genStruct spec msg)
  require impl
  return funName
  where
    impl = do
      pureStms <- mkStms
      cfun [i|
        void ${funName} (struct ${structName} *dst, char const *buf) {
          ${concat pureStms}
        }
      |]
    funName :: Identifier = [qc|read_{structName}|]
    structName        = cnm $ _msgName msg
    ofsts             = scanl (+) 0 (_len <$> _fields msg)
    read ofst field   = _readMember
      [i|dst->${cnm (_name field)}|]
      [i|buf + ${ofst}|]
      field
    mkStms            = zipWithM read ofsts (_fields msg)
 
mainLoop :: Specification a -> MsgHandler a -> C Identifier
mainLoop spec@(Specification {..}) handler@(MsgHandler {..}) = do
  include "stdio.h"

  structs :: [Code] <- forM (_outgoingMessages _proto) $ \msg -> do
    struct <- require $ genStruct spec msg
    return [i|struct ${cnm $ _msgName msg} ${cnm $ _msgName msg}|]

  cases :: [Code] <- forM (_outgoingMessages _proto) $ \msg -> do
    readMsg   <- readStruct spec msg
    struct    <- require $ genStruct spec msg
    handleMsg <- _handleMsg msg
    let prefix = Exp [i|msg.${cnm $ _msgName msg}|]
 
    return [i|case ${_tag msg} : {
 
      if (fread(buf, 1, ${bodyLen _proto msg}, stdin) == 0) {
        return -1;
      }
      /* parse struct */
      ${readMsg} (&msg.${cnm $ _msgName msg}, buf);
 
      ${handleMsg}
 
      break;
 
    }|]

  include "cassert"
  let
    readHeader = if _pktHdrLen _proto > 0
      then [i|
        if (fread(buf, 1, ${_pktHdrLen _proto}, stdin) == 0) {
          return -1;
        }|]
      else []
    funName = "handle"

  cfun [i|
    int ${funName}(char *buf) {
      union {
        ${intercalate "\n" $ (++";") `map` structs}
      } msg;
      (void)0;/* Read the packet header if any */
      ${readHeader}
      (void)0;/* Read the packet type */
      if (fread(buf, 1, 1, stdin) == 0) {
        return -1;
      }
      switch (*buf) {
        ${intercalate "\n" cases}
        default: {
          assert(false);
          return -1;
        }
      }
      return 1;
    }
  |]
  return (Identifier funName)
 
cmain :: Specification a -> MsgHandler a -> C C.Func
cmain spec@(Specification {..}) handler@(MsgHandler {..}) = do
  include "cstdio"
  include "cmath" -- lazy, just assume the code might use it
  loopStep     <- mainLoop spec handler
  initMsgs     <- _initMsg `mapM` _outgoingMessages _proto
  cleanupMsgs  <- _cleanupMsg `mapM` _outgoingMessages _proto
 
  cfun [i|int main(int argc, char **argv) {
    ${concat initMsgs}
    char buf[${bufLen}];
    int ret = 0;
    int pkts = 0;
 
    while(ret >= 0) {
      ret = ${loopStep}(buf);
      ++pkts;
    }
 
    fprintf(stderr, "Cleaning up.\\n");
    ${concat cleanupMsgs}
    fprintf(stderr, "%d packets\\n", pkts);
 
  }|]
  where
    bufLen     = maximum $ foreach (_outgoingMessages _proto) $
      rotateL (1::Int) . (+1) . logBase2 . bodyLen _proto
 
    logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

codeGen :: Bool -> C a -> String
codeGen dbg code = clang_format . s 80 . ppr $ mkCompUnit code
  where
    s = if dbg
      then prettyPragma
      else pretty

clang_format :: String -> String
clang_format = unsafePerformIO . readProcess "clang-format"
  [ "-assume-filename=cpp"
  , "-style={BasedOnStyle: Google, BreakBeforeBraces: Linux, NamespaceIndentation: All}"
  ]

type Debug = Bool

data CCompiler = GCC | Clang

compile :: CompileOptions -> FilePath -> C a -> IO ()
compile (CompileOptions dbg optLevel compiler oname) buildDir code = do
  timer "codegen" $ writeFile src (codeGen False code)
  hPutStrLn stderr $ "## " ++ cmd
  timer "compile" $ callCommand cmd
  where
    cmd = [qc|{cc} -std=c++11 -march=native -O{optLevel} {dbgFlag} -o {out} {src}|]
    cc  = compilerCmd compiler
    src :: String = [qc|{buildDir}/{oname}.cpp|]
    out :: String = [qc|{buildDir}/{oname}|]
    dbgFlag = if dbg then "-g" else ""

compilerCmd :: CCompiler -> String
compilerCmd compiler = case compiler of
  GCC   -> "g++"
  Clang -> "clang++"

data CompileOptions = CompileOptions
  { debug    :: Debug
  , optLevel :: Int
  , compiler :: CCompiler
  , filename :: FilePath
  }
 
compileShake :: CompileOptions -> FilePath -> C a -> Rules ()
compileShake (CompileOptions dbg optLevel compiler oname) buildDir code = do
 
  let
    src = [qc|{buildDir}/{oname}.cpp|]
    out = [qc|{buildDir}/{oname}|]
    cc  = compilerCmd compiler
 
  src %> \out -> do
    alwaysRerun
    writeFileChanged out $ codeGen dbg code
 
  out %> \out -> do
 
    need [src]
 
    let dbgFlag = switch "-g" "" dbg
    command_ [Cwd buildDir, Shell] -- Compile
      [qc|{cc} -std=c++11 -march=native -O{optLevel} {dbgFlag} -o {oname} {oname}.cpp|] []
 
    command_ [Cwd buildDir, Shell] -- Grab the demangled assembly
      [qc|objdump -Cd {oname} > {oname}.s|] []
 

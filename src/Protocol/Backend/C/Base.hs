{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Protocol.Backend.C.Base where

import qualified Language.C.Syntax as C
import           Language.C.Quote.C

import           Language.C.Utils as C

import           Protocol

import           Development.Shake

import           Text.PrettyPrint.Mainland (putDocLn, ppr, pretty, prettyPragma)
import           Text.InterpolatedString.Perl6

import           Data.Bits

import           Utils
import           Data.Monoid

import           Control.Monad

import           System.IO.Unsafe (unsafePerformIO)
import           System.Process

data Specification a = Specification
  { _proto      :: Proto a
  , _mkTy       :: Field a -> C C.GType
  , _readMember :: C.Exp -> C.Exp -> Field a -> C C.Stm
  }

data MsgHandler a = MsgHandler
  { _handleMsg  :: Message a -> C [C.Stm]
  , _initMsg    :: Message a -> C [C.Stm]
  , _cleanupMsg :: Message a -> C [C.Stm]
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

cstruct :: String -> [C.FieldGroup] -> C C.Type
cstruct name membs = do
  depends [cty|struct $id:name {
    $sdecls:membs
  }|]
  return [cty|typename $id:("struct "++name)|]

cnm :: String -> String
cnm = rawIden . cname

genStruct :: Specification a -> Message a -> C C.Type
genStruct spec msg = do
  decls <- runDecls
  cstruct (cnm $ _msgName msg) decls
  where
    runDecls = mapM declare (_fields msg)
    declare f@(Field _ len nm ty _) = mkCsdecl (rawIden $ cname nm) <$> mkTy f
    mkTy = _mkTy spec

mkCsdecl :: String -> C.GType -> C.FieldGroup                                   
mkCsdecl nm (C.SimpleTy ty)   = [csdecl|$ty:ty $id:nm;|]
mkCsdecl nm (C.ArrayTy ty sz) = [csdecl|$ty:ty $id:nm[$sz]; |]
 
readStruct :: Specification a -> Message a -> C C.Func
readStruct spec@(Specification {..}) msg = do
  include "cstring"
  require (genStruct spec msg)
  require impl
  where
    impl = do
      pureStms <- stms
      depends [cfun|
        void $id:(funName) (struct $id:(structName) *dst, char const *buf) {
          $stms:pureStms
        }
      |]
    funName :: String = [qc|read_{structName}|]
    structName        = cnm $ _msgName msg
    ofsts             = scanl (+) 0 (_len <$> _fields msg)
    read ofst field   = _readMember
      [cexp|dst->$id:(cnm (_name field))|]
      [cexp|buf + $ofst|]
      field
    stms              = zipWithM read ofsts (_fields msg)
 
mainLoop :: Specification a -> MsgHandler a -> C C.Func
mainLoop spec@(Specification {..}) handler@(MsgHandler {..}) = do
  include "stdio.h"

  structs <- forM (_outgoingMessages _proto) $ \msg -> do
    struct <- require $ genStruct spec msg
    return [csdecl|struct $id:(cnm $ _msgName msg) $id:(cnm $ _msgName msg);|]

  cases <- forM (_outgoingMessages _proto) $ \msg -> do
    readMsg   <- readStruct spec msg
    struct    <- require $ genStruct spec msg
    handleMsg <- _handleMsg msg
    let prefix = [cexp|msg.$id:(cnm $ _msgName msg)|]
 
    return [cstm|case $exp:(_tag msg) : {
 
      if (fread(buf, 1, $(bodyLen _proto msg), stdin) == 0) {
        return -1;
      }
      /* parse struct */
      $id:(getId readMsg) (&msg.$id:(cnm $ _msgName msg), buf);
 
      $stms:(handleMsg)
 
      break;
 
    }|]

  include "cassert"
  let readHeader = if _pktHdrLen _proto > 0
        then [cstms|
          if (fread(buf, 1, $(_pktHdrLen _proto), stdin) == 0) {
            return -1;
          }|]
        else []
  depends [cfun|
    int handle(char *buf) {
      union {
        $sdecls:structs
      } msg;
      (void)0;/* Read the packet header if any */
      $stms:readHeader
      (void)0;/* Read the packet type */
      if (fread(buf, 1, 1, stdin) == 0) {
        return -1;
      }
      switch (*buf) {
        $stms:cases
        default: {
          assert(false);
          return -1;
        }
      }
      return 1;
    }
  |]
 
cmain :: Specification a -> MsgHandler a -> C C.Func
cmain spec@(Specification {..}) handler@(MsgHandler {..}) = do
  include "cstdio"
  loopStep     <- mainLoop spec handler
  initMsgs     <- _initMsg `mapM` _outgoingMessages _proto
  cleanupMsgs  <- _cleanupMsg `mapM` _outgoingMessages _proto
 
  depends [cfun|int main(int argc, char **argv) {
    $stms:(concat initMsgs)
    char buf[$bufLen];
    int ret = 0;
    int pkts = 0;
 
    while(ret >= 0) {
      ret = $id:(getId loopStep) (buf);
      ++pkts;
    }
 
    fprintf(stderr, "Cleaning up.\n");
    $stms:(concat cleanupMsgs)
    fprintf(stderr, "%d packets\n", pkts);
 
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

compile :: Debug -> CCompiler -> FilePath -> FilePath -> C a -> IO ()
compile dbg compiler buildDir oname code = do
  writeFile src (codeGen False code)
  putStrLn $ "## " ++ cmd
  callCommand cmd
  where
    cmd = [qc|{cc} -std=c++11 -march=native -O2 {dbgFlag} -o {out} {src}|]
    cc  = compilerCmd compiler
    src :: String = [qc|{buildDir}/{oname}.cpp|]
    out :: String = [qc|{buildDir}/{oname}|]
    dbgFlag = if dbg then "-g" else ""

compilerCmd :: CCompiler -> String
compilerCmd compiler = case compiler of
  GCC   -> "g++"
  Clang -> "clang++"
 
compileShake :: Debug -> CCompiler -> FilePath -> FilePath -> C a -> Rules ()
compileShake dbg compiler buildDir oname code = do
 
  let
    src = [qc|{buildDir}/{oname}.cpp|]
    out = [qc|{buildDir}/{oname}|]
    cpp = compilerCmd compiler
 
  src %> \out -> do
    alwaysRerun
    writeFileChanged out $ codeGen dbg code
 
  out %> \out -> do
 
    need [src]
 
    let dbgFlag = switch "-g" "" dbg
    command_ [Cwd buildDir, Shell] -- Compile
      [qc|{cpp} -std=c++11 -march=native -O2 {dbgFlag} -o {oname} {oname}.cpp|] []
 
    command_ [Cwd buildDir, Shell] -- Grab the demangled assembly
      [qc|objdump -Cd {oname} > {oname}.s|] []
 

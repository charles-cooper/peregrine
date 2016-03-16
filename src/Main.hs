{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Protocol.Tmx.TAQ as TAQ
import Protocol

import           Language.C.Quote.C
import           Language.C.Smart ()
import qualified Language.C.Syntax as C
import qualified Language.C.Utils as C
import           Language.C.Utils (C, CompUnit, depends, include, noloc)
import           Language.C.Utils (topDecl)

import Text.PrettyPrint.Mainland (putDocLn, ppr, pretty, prettyPragma)
import Text.InterpolatedString.Perl6 (qc)

import Data.List (nub, sort, intercalate)
import Utils

import Data.Bits

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

cppstr :: C (C.Type)
cppstr = do
  include "string"
  return [cty|typename $id:("std::string")|]

assert :: Bool -> a -> a
assert pred a = if pred then a else (error "Assertion failed")

mkCsdecl :: String -> C.GType -> C.FieldGroup
mkCsdecl nm (C.SimpleTy ty)   = [csdecl|$ty:ty $id:nm;|]
mkCsdecl nm (C.ArrayTy ty sz) = [csdecl|$ty:ty $id:nm[$sz]; |]

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

readStruct :: Message TAQ -> C C.Func
readStruct msg = do
  depends $ genStruct msg
  include "cstring"
  impl
  where
    impl = do
      pureStms <- stms
      depends [cfun|
        void $id:(funName) (struct $id:(structName) *dst, char const *buf) {
          $stms:pureStms
        }
      |]
    funName :: String = [qc|read_{msg^.msgName}|]
    structName        = msg^.msgName
    ofsts             = scanl (+) 0 (msg^.fields & map _len)

    readMember ofst f@(Field _ len nm ty _) = case ty of

      Numeric    -> runIntegral

      Alphabetic -> return $ if len == 1
                      then [cstm|dst->$id:cnm = *$buf;|]
                      else [cstm|memcpy(&dst->$id:cnm, $buf, $len);|]

      Boolean    -> return [cstm|dst->$id:cnm = (*$buf == '1');|]

      Date       -> runIntegral
      Time       -> runIntegral
      where
        buf = [cexp|buf + $ofst|]
        runIntegral = do
          dep <- cReadIntegral (C.simplety $ mkTy f)
          return [cstm|dst->$id:cnm = $id:(getId dep) ($buf, $len); |]

        cnm     = rawIden (cname nm)

    stms = zipWithM readMember ofsts (msg^.fields)

unId :: C.Id -> String
unId (C.Id str _)     = str
unIt (C.AntiId str _) = str

class Identifiable a where
  getId :: a -> C.Id

instance Identifiable C.Func where
  getId (C.Func _ iden _ _ _ _)      = iden
  getId (C.OldFunc _ iden _ _ _ _ _) = iden
-- thi sisn't exactly right ..
instance Identifiable C.Type where
  getId t@(C.Type (C.DeclSpec _ _ ty _) _ _) = case ty of
    C.Tstruct (Just id) _ _ _ -> id
    C.Tunion  (Just id) _ _ _ -> id
    C.Tenum   (Just id) _ _ _ -> id
    _ -> error $ "Not identifiable: " ++ show t
  getId t = error $ "Not identifiable: " ++ show t

mkCompUnit :: C a -> [C.Definition]
mkCompUnit code = [cunit|
    /* Includes */
    $edecls:(nub $ include <$> incls)

    /* Other definitions */
    $edecls:(nub $ defs)

    /* Types */
    $edecls:(typedecl <$> nub tys)

    /* Top level declarations */
    $edecls:(declare <$> nub decls)

    /* Functions */
    $edecls:(fundecl <$> nub funcs)
  |]
  where
    include (C.Includes header)  = [cedecl|$esc:includeStr|]
      where includeStr = [qc|#include <{header}>|]
    typedecl ty                  = [cedecl|$ty:ty;|]
    fundecl func                 = [cedecl|$func:func|]
    declare decl                 = [cedecl|$decl:decl;|]
    (C.CompUnit incls defs tys funcs decls) = C.getCompUnit code

cReadIntegral ty = do
  mapM include ["cstdint", "cassert", "cctype"]
  depends impl
  where
    funName :: String = [qc|parse_{pretty 0 $ ppr ty}|]
    impl = [cfun|
      $ty:ty $id:(funName) (char const *buf, $ty:uint len) {
        $ty:ty ret = 0;
        while (len--) {
          assert(isdigit(*buf));
          ret = ret * 10 + (*buf - '0');
          ++buf;
        }
        return ret;
      }
    |]

msgLen :: Message TAQ -> Int
msgLen msg = sum $ _len <$> _fields msg

printFmt :: C.Exp -> Message TAQ -> (String, [C.Exp])
printFmt root msg = let
  (fmts, ids) = unzip $ fmt <$> _fields msg
  in (intercalate ", " fmts, mconcat ids)
  where
    fmt (Field _ len nm ty _)
      | ty==Numeric     = ([qc|{nm}: %d|], [exp nm])
      | ty==Alphabetic  = ([qc|{nm}: %.{len}s|], [[cexp|&$(exp nm)|]])
      | ty==Boolean     = ([qc|{nm}: %d|], [exp nm])
      | ty==Date        = ([qc|{nm}: %d|], [exp nm])
      | ty==Time        = ([qc|{nm}: %d|], [exp nm])
    exp nm = [cexp|$root.$id:(rawIden $ cname nm)|]

mainLoop :: C C.Func
mainLoop = do
  include "stdio.h"
  cases <- forM (_outgoingMessages taq) $ \msg -> do
    readMsg <- readStruct msg
    struct <- depends $ genStruct msg
    let prefix = [cexp|msg.$id:(getId struct)|]
    let printf = printFmt prefix msg

    return [cstm|case $exp:(_tag msg) : {

      // read bytes
      if (fread(buf, 1, $(msgLen msg + 1), stdin) == 0) {
        return -1;
      }

      // parse struct
      $id:(getId readMsg) (&msg.$id:(getId struct), buf);

      // write struct
      write($id:(fdName msg).writefd,
          &msg.$id:(getId struct),
          sizeof(struct $id:(getId struct)));

      /*printf($(fst printf ++ "\n"), $args:(snd printf));*/
      break;

    }|]
  structs <- forM (_outgoingMessages taq) $ \msg -> do
    struct <- depends $ genStruct msg
    return [csdecl|struct $id:(getId struct) $id:(getId struct);|]

  depends [cfun|
    int handle(char *buf) {
      union {
        $sdecls:structs
      } msg;
      if (fread(buf, 1, 1, stdin) == 0) {
        return -1;
      }
      switch (*buf) {
        $stms:cases
        default: {
          assert("Failed to read tag.");
          return -1;
        }
      }
      return 1;
    }
  |]

fdName :: Message a -> String
fdName msg = rawIden $ cname $ _msgName msg

initMsg :: Message a -> C C.Stm
initMsg msg = do
  mapM include
    ["cstdio", "cstdlib", "unistd.h", "sys/types.h", "sys/wait.h", "fcntl.h"]
  depends [cty|struct spawn { int writefd; int pid; }|]
  topDecl [cdecl|struct spawn $id:(fdName msg);|]
  depends [cfun|struct spawn spawn_child(char const *filename, char const *progname, char const *argv0) {
    int fds[2];
    int ret;
    if ((ret = pipe(fds)) < 0) {
      perror("pipe");
      exit(ret);
    }
    int readfd  = fds[0];
    int writefd = fds[1];

    int cpid = fork();
    if (cpid < 0) {
      perror("fork");
      exit(1);
    }
    /* parent */
    if (cpid) {
      close(readfd);
      struct spawn ret;
      ret.writefd = writefd;
      ret.pid     = cpid;
      return ret;
    } else {
      close(writefd);
      /* stdin from pipe */
      if ((ret = dup2(readfd, STDIN_FILENO)) < 0) {
        perror("dup2");
        exit(ret);
      }
      int outfd = open(filename, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
      if (outfd < 0) {
        perror("open output");
        exit(outfd);
      }
      /* pipe to file */
      if ((ret = dup2(outfd, STDOUT_FILENO)) < 0) {
        perror("dup2");
        exit(ret);
      }
      if ((ret = execlp(progname, argv0, 0)) < 0) {
        perror("execlp");
        exit(ret);
      }
    }
  }|]
  str <- require cppstr
  let mkStr = [cexp|$id:("std::string")|]
  return [cstm|{
    $ty:str filename = $mkStr("out/")
        + $mkStr(argv[1]) + $mkStr($(outputName++""));
    $id:(fdName msg) =
      spawn_child(filename.c_str(), $codecStr, $(codecStr ++ auxName));
    }|]
  where
    outputName = [qc|.{fdName msg}.{suffix}|]
    auxName    = [qc| ({fdName msg})|]
    suffix     = "gz"
    codecStr   = "gzip"

require = id

cleanupMsg :: Message a -> C [C.Stm]
cleanupMsg msg = do
  initMsg msg -- pull dependencies
  return [cstms|close  ($id:(fdName msg).writefd);
              /* don't wait for them or else strange hang occurs.
               * without waiting, there is a small race condition between
               * when the parent finishes and when the gzip children finish.
               * fprintf(stderr, "Waiting %d\n", $id:(fdName msg).pid);
               * waitpid($id:(fdName msg).pid, NULL, 0); */
                ;
        |]

cmain :: C C.Func
cmain = do
  include "cstdio"
  loopStep     <- mainLoop
  outputPrefix <- topDecl [cdecl|char const *outputPrefix;|]
  initMsgs     <- initMsg `mapM` _outgoingMessages taq
  cleanupMsgs  <- cleanupMsg `mapM` _outgoingMessages taq

  depends [cfun|int main(int argc, char **argv) {
    $stms:(initMsgs)
    char buf[$bufLen];
    int ret = 0;
    int pkts = 0;

    while(ret >= 0) {
      ret = $id:(getId loopStep) (buf);
      ++pkts;
      if (pkts % 1000000 == 0) {
        fprintf(stderr, "Checkpoint! %d\n", pkts);
      }
    }

    fprintf(stderr, "Cleaning up.\n");
    $stms:(concat cleanupMsgs)
    fprintf(stderr, "%d packets\n", pkts);

  }|]
  where
    bufLen     = maximum $ foreach (_outgoingMessages taq) $
      rotateL (1::Int) . (+1) . logBase2 . msgLen

    logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

----------------------------
-- Compilation
----------------------------

compile :: C a -> Rules ()
compile code = do

  let buildDir = "bin"
  let src = [qc|{buildDir}/main.cpp|]
  let out = [qc|{buildDir}/a.out|]

  src %> \out -> do
    alwaysRerun
    writeFileChanged out (pretty 80 (ppr (mkCompUnit code)))

  out %> \out -> do

    need [src]

    command_ [Cwd buildDir, Shell] -- Compile
      [qc|g++ -std=c++11 -march=native -O2 -g main.cpp|] []

    command_ [Cwd buildDir, Shell] -- Grab the demangled assembly
      [qc|objdump -Cd a.out > main.s|] []

main = do
  shakeArgs shakeOptions $ do

    compile cmain

    let exe = "bin/a.out"

    "data/*.lz4" %> \out -> do
      let src = "/mnt/efs/ftp/tmxdatalinx.com" </> (takeFileName out) -<.> "gz"
      need [src]
      command [Shell] [qc|gzip -d < {src} | lz4 -9 > {out}|] []

    let date = "20150826"
    phony "Run a.out" $ do
      let dataFile = [qc|data/{date}TradesAndQuotesDaily.lz4|]
      need [dataFile, exe]
      command_ [Shell] [qc|lz4 -d < {dataFile} | {exe} {date} |] []

    want ["bin/a.out"]



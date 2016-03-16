{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Protocol.Backend.C.Base where

import qualified Language.C.Syntax as C
import           Language.C.Quote.C

import           Language.C.Utils as C

import           Protocol

import           Development.Shake

import Text.PrettyPrint.Mainland (putDocLn, ppr, pretty, prettyPragma)
import Text.InterpolatedString.Perl6 (qc)

import           Data.Bits
import           Utils
import           Control.Monad

data Specification a = Specification
  { _mkTy       :: Field a -> C C.GType
  , _readMember :: Int -> Field a -> C C.Stm
  , _handleMsg  :: Message a -> C C.Stm
  , _initMsg    :: Message a -> C C.Stm
  , _cleanupMsg :: Message a -> C C.Stm
  }

genStruct :: (Field a -> C C.GType) -> Message a -> C C.Type
genStruct mkTy msg = do
  decls <- runDecls
  depends [cty|
      struct $id:(_msgName msg) {
        $sdecls:decls
      }
    |]
  where
    runDecls = mapM declare (_fields msg)
    declare f@(Field _ len nm ty _) = mkCsdecl (rawIden $ cname nm) <$> mkTy f

mkCsdecl :: String -> C.GType -> C.FieldGroup                                   
mkCsdecl nm (C.SimpleTy ty)   = [csdecl|$ty:ty $id:nm;|]
mkCsdecl nm (C.ArrayTy ty sz) = [csdecl|$ty:ty $id:nm[$sz]; |]
 
readStruct :: Specification a -> Message a -> C C.Func
readStruct spec@(Specification {..}) msg = do
  include "cstring"
  require (genStruct _mkTy msg)
  require impl
  where
    impl = do
      pureStms <- stms
      depends [cfun|
        void $id:(funName) (struct $id:(structName) *dst, char const *buf) {
          $stms:pureStms
        }
      |]
    funName :: String = [qc|read_{_msgName msg}|]
    structName        = _msgName msg
    ofsts             = scanl (+) 0 (_len <$> _fields msg)
    stms              = zipWithM _readMember ofsts (_fields msg)
 
mainLoop :: Specification a -> Proto a -> C C.Func
mainLoop spec@(Specification {..}) proto = do
  include "stdio.h"
  cases <- forM (_outgoingMessages proto) $ \msg -> do
    readMsg   <- readStruct spec msg
    struct    <- require $ genStruct _mkTy msg
    handleMsg <- _handleMsg msg
    let prefix = [cexp|msg.$id:(getId struct)|]
 
    return [cstm|case $exp:(_tag msg) : {
 
      /* read bytes */
      if (fread(buf, 1, $(bodyLen proto msg), stdin) == 0) {
        return -1;
      }
      /* parse struct */
      $id:(getId readMsg) (&msg.$id:(getId struct), buf);
 
      $stm:(handleMsg)
 
      break;
 
    }|]
  structs <- forM (_outgoingMessages proto) $ \msg -> do
    struct <- require $ genStruct _mkTy msg
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
 
cmain :: Specification a -> Proto a -> C C.Func
cmain spec@(Specification {..}) proto = do
  include "cstdio"
  loopStep     <- mainLoop spec proto
  -- outputPrefix <- topDecl [cdecl|char const *outputPrefix;|]
  initMsgs     <- _initMsg `mapM` _outgoingMessages proto
  cleanupMsgs  <- _cleanupMsg `mapM` _outgoingMessages proto
 
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
    $stms:(cleanupMsgs)
    fprintf(stderr, "%d packets\n", pkts);
 
  }|]
  where
    bufLen     = maximum $ foreach (_outgoingMessages proto) $
      rotateL (1::Int) . (+1) . logBase2 . bodyLen proto
 
    logBase2 x = finiteBitSize x - 1 - countLeadingZeros x
 
compile :: String -> C a -> Rules ()
compile buildDir code = do
 
  let src = [qc|{buildDir}/main.cpp|]
  let out = [qc|{buildDir}/a.out|]
 
  src %> \out -> do
    alwaysRerun
    writeFileChanged out (prettyPragma 80 (ppr (mkCompUnit code)))
 
  out %> \out -> do
 
    need [src]
 
    command_ [Cwd buildDir, Shell] -- Compile
      [qc|g++ -std=c++11 -march=native -O2 -g main.cpp|] []
 
    command_ [Cwd buildDir, Shell] -- Grab the demangled assembly
      [qc|objdump -Cd a.out > main.s|] []
 


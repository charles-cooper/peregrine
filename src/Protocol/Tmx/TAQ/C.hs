{-# LANGUAGE QuasiQuotes #-}
module Protocol.Tmx.TAQ.C where
import Protocol
import Protocol.Tmx.TAQ
import Protocol.Backend.C.Base as C
import Language.C.Utils as C
import Text.InterpolatedString.Perl6 (q, qc)
import qualified Language.C.Syntax as C
import Language.C.Quote.C
import Language.C.Lib
import Utils
import Data.List (intercalate)

-- specification for c implementation

taqCSpec :: C.Specification TAQ
taqCSpec = C.Specification
  { _proto      = taq
  , _mkTy       = mkTy
  , _readMember = readMember
  }
 
symbol :: C C.Type
symbol = do
  include "functional"
  depends [cty|struct symbol { $ty:ulong symbol; }|]
  hash <- depends [cedecl|$esc:esc|]
  return [cty|typename $id:("struct symbol")|]
  where
    esc = dedent [q|
      namespace std {
        template <> struct hash<struct symbol> {
          uint64_t operator()(const struct symbol& k) const {
            return hash<uint64_t>()(k.symbol);
          }
        };
        template <> struct equal_to<struct symbol> {
          bool operator() (const struct symbol& x, const struct symbol& y)
            const
          {
            return equal_to<uint64_t>()(x.symbol, y.symbol);
          }
        };
      }
    |]

mkTy :: Field TAQ -> C C.GType
mkTy f@(Field _ len _ ty _) = do
  symbol__ <- require symbol
  let ret
        | ty==Numeric    && len==9 = C.SimpleTy uint
        | ty==Numeric    && len==7 = C.SimpleTy uint
        | ty==Numeric    && len==3 = C.SimpleTy ushort
        | ty==Numeric              = error $ "Unknown integer len for " ++ show f
        | ty==Alphabetic && len==1 = C.SimpleTy char
        | ty==Alphabetic           = C.SimpleTy symbol__
        | ty==Boolean              = assert (len==1) $ C.SimpleTy bool
        | ty==Date                 = C.SimpleTy uint -- assert?
        | ty==Time                 = C.SimpleTy uint -- assert?
        | otherwise                = error "Unknown case."
  return ret

readMember dst src f@(Field _ len nm ty _) = case ty of
  Numeric    -> runIntegral
  Alphabetic -> return $ if len == 1
                  then [cstm|$dst = *$src;|]
                  else [cstm|memcpy(&$dst, $src, $len);|]
  Boolean    -> return [cstm|$dst = (*$src == '1');|]
  Date       -> runIntegral
  Time       -> runIntegral
  where
    runIntegral = do
      readInt <- cReadIntegral =<< (C.simplety <$> mkTy f)
      return [cstm|$dst = $id:(C.getId readInt) ($src, $len); |]

handleMsgGzip :: Message TAQ -> C C.Stm
handleMsgGzip msg = do
  struct <- require $ genStruct taqCSpec msg
  return [cstm|write($id:(fdName msg).writefd,
          &msg.$id:(_msgName msg),
          sizeof(struct $id:(_msgName msg)));|]
  where
    cnm = rawIden . cname . _msgName

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

fdName :: Message a -> String
fdName msg = rawIden $ cname $ _msgName msg

initMsgGzip :: Message a -> C C.Stm
initMsgGzip msg = do
  str <- require cpp_string
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
  str <- require cpp_string
  let mkStr = [cexp|$id:("std::string")|]
  return [cstm|{
    $ty:str filename = $mkStr("out/")
        + $mkStr(argv[1]) + $mkStr($(outputName :: String));
    $id:(fdName msg) =
      spawn_child(filename.c_str(), $codecStr, $(codecStr ++ auxName));
    }|]
  where
    outputName = [qc|.{fdName msg}.{suffix}|]
    auxName    = [qc| ({fdName msg})|]
    suffix     = "gz"
    codecStr   = "gzip"

cleanupMsgGzip :: Message a -> C C.Stm
cleanupMsgGzip msg = do
  initMsgGzip msg -- pull dependencies
  return [cstm|{close  ($id:(fdName msg).writefd);
              /* don't wait for them or else strange hang occurs.
               * without waiting, there is a small race condition between
               * when the parent finishes and when the gzip children finish.
               * fprintf(stderr, "Waiting %d\n", $id:(fdName msg).pid);
               * waitpid($id:(fdName msg).pid, NULL, 0); */
                ;}|]



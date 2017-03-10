{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module Protocol.Tmx.TAQ.C where
import Protocol
import Protocol.Tmx.TAQ
import Protocol.Backend.C.Base as C
import Language.C.Utils as C
import Text.InterpolatedString.Perl6 (q, qc)
import Data.String.Interpolate.IsString
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
  cty [i|struct symbol { ${ulong} symbol; }|]
  hash <- cty stdStuff
  -- ^ use cty to get CompUnit to put it right after struct definition
  return $ Type [i|struct symbol|]
  where
    stdStuff = [i|
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

mkTy :: Field TAQ -> C C.Type
mkTy f@(Field _ len _ ty _) = do
  ret <- if
    | ty==Numeric    && len==9 -> pure uint
    | ty==Numeric    && len==7 -> pure uint
    | ty==Numeric    && len==3 -> pure ushort
    | ty==Numeric              -> error $ "Unknown integer len for " ++ show f
    | ty==Alphabetic && len==1 -> pure char
    | ty==Alphabetic           -> symbol
    | ty==Boolean              -> pure $ assert (len==1) $ bool
    | ty==Date                 -> pure uint -- assert?
    | ty==Time                 -> pure uint -- assert?
    | otherwise                -> error "Unknown case."
  return ret

-- C function to convert HHMMSS00 to millis past midnight
readTime :: C Func
readTime = do
  let fname = Func "read_taq_time"
  cfun [i|
    ${uint} ${fname}(${uint} const t) {
      // HHMMSS00
      ${uint} const hh = t / 1000000;
      ${uint} const mm = (t / 10000) % 100;
      ${uint} const ss = (t / 100) % 100;
      ${uint} const cs = t % 100;/*centi-seconds*/
      return (cs * 10)
        + (ss * 1000)
        + (mm * 60 * 1000)
        + (hh * 60 * 60 * 1000);
    }
    |]
  return fname

readMember :: Exp -> Exp -> Field TAQ -> C Code
readMember dst src f@(Field _ len nm ty _) = case ty of
  Numeric    -> runIntegral
  Alphabetic -> return $ if len == 1
                  then [i|${dst} = *${src};|]
                  else [i|memcpy(&${dst}, ${src}, ${len});|]
  Boolean    -> return [i|${dst} = (*${src} == '1');|]
  Date       -> runTime
  Time       -> runIntegral
  where
    runIntegral = do
      readInt <- cReadIntegral =<< mkTy f
      return [i|${dst} = ${readInt} (${src}, ${len}); |]
    runTime = do
      readTime <- readTime
      readInt <- cReadIntegral uint
      return [i|${dst} = ${readTime}(${readInt}(${src}, ${len})); |]

handleMsgGzip :: Message TAQ -> C Code
handleMsgGzip msg = do
  struct <- require $ genStruct taqCSpec msg
  return [i|write(${fdName msg}.writefd,
          &msg.${_msgName msg},
          sizeof(struct ${_msgName msg}));|]
  where
    cnm = rawIden . cname . _msgName

printFmt :: C.Exp -> Message TAQ -> (String, [C.Exp])
printFmt root msg = let
  (fmts, ids) = unzip $ fmt <$> _fields msg
  in (intercalate ", " fmts, mconcat ids)
  where
    fmt (Field _ len nm ty _)
      | ty==Numeric     = ([qc|{nm}: %d|], [exp])
      | ty==Alphabetic  = ([qc|{nm}: %.{len}s|], [[i|&${exp}|]])
      | ty==Boolean     = ([qc|{nm}: %d|], [exp])
      | ty==Date        = ([qc|{nm}: %d|], [exp])
      | ty==Time        = ([qc|{nm}: %d|], [exp])
      | otherwise       = error "Impossible case"
      where
        exp = [i|${root}.${cnm nm}|]

fdName :: Message a -> String
fdName msg = rawIden $ cname $ _msgName msg

initMsgGzip :: Message a -> C Code
initMsgGzip msg = do
  str <- require cpp_string
  mapM include
    ["cstdio", "cstdlib", "unistd.h", "sys/types.h", "sys/wait.h", "fcntl.h"]
  cty [i|struct spawn { int writefd; int pid; }|]
  topDecl [i|struct spawn ${fdName msg};|]
  cfun [i|struct spawn spawn_child(char const *filename, char const *progname, char const *argv0) {
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
  return [i|{
    std::string filename = std::string ("out/")
        + std::string(argv[1]) + std::string(${outputName});
    ${fdName msg} =
      spawn_child(filename.c_str(), ${codecStr}, ${codecStr ++ auxName});
    }|]
  where
    outputName :: String = [qc|.{fdName msg}.{suffix}|]
    auxName    :: String = [qc| ({fdName msg})|]
    suffix     = "gz"
    codecStr   = "gzip"

cleanupMsgGzip :: Message a -> C Code
cleanupMsgGzip msg = do
  initMsgGzip msg -- pull dependencies
  return [i|{close  (${fdName msg}.writefd);
              /* don't wait for them or else strange hang occurs.
               * without waiting, there is a small race condition between
               * when the parent finishes and when the gzip children finish.
               * fprintf(stderr, "Waiting %d\n", ${fdName msg}.pid);
               * waitpid(${fdName msg}.pid, NULL, 0); */
                ;}|]



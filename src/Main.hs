{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Protocol.Tmx.TAQ as TAQ
import Protocol
import Protocol.Backend.C.Base as C

import           Language.C.Quote.C
import qualified Language.C.Syntax as C
import qualified Language.C.Smart as C ((+=))
import qualified Language.C.Utils as C
import           Language.C.Utils (C, CompUnit, depends, include, noloc)
import           Language.C.Utils (topDecl, require)
import           Language.C.Utils (char, bool, ushort, uint, ulong)

import Data.List (intercalate, sort, nub)
import Data.Monoid
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Char (isSpace)

import Text.PrettyPrint.Mainland (putDocLn, ppr, pretty, prettyPragma, Pretty(..))
import Text.InterpolatedString.Perl6 (q, qc)

import Data.Text as T (unpack)

import Utils

import Development.Shake
import Development.Shake.FilePath

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map as Map
import           Data.Map (Map(..))
import qualified Data.Set as Set
import           Data.Set (Set(..))

import Data.Function

data Void

data Merge a
  = Tip (Message a)
  | Node String (Map String (Merge a))

data Projection a
  = Projection { projNames :: [String], projsrc :: (Merge a) }

data Op = Add | Mul

data Fold a
  = Foldl { foldop :: Op, foldsrc :: a } -- TODO initializer?

data Zip a b = ZipWith { zipop :: Op, zipsrca :: a, zipsrcb :: b }

data AST a
  = ZipExp (Zip (AST a) (AST a))
  | FoldExp (Fold (AST a))
  | ProjectExp (Projection a)

set :: Ord a => [a] -> Set a
set = Set.fromList

compileOp :: Op -> (C.Exp -> C.Exp -> C.Exp)
compileOp Add = (+)
compileOp Mul = (*)

compileAST :: C.Specification TAQ
  -> C.MsgHandler TAQ
  -> AST TAQ
  -> C (ASTState TAQ)
compileAST spec handler ast = case ast of
  FoldExp (Foldl op ast) -> do
    st <- compileAST spec handler ast
    compileFold (Foldl op st)
  ZipExp (ZipWith op ast1 ast2) -> do
    st1 <- compileAST spec handler ast1
    st2 <- compileAST spec handler ast2
    compileZip (ZipWith op st1 st2)
  ProjectExp p -> compileProjection spec handler p

data ASTState a = ASTState
  { src      :: String
  , deps     :: Set (Message a)
  , handlers :: C.MsgHandler a
  }

compileFold :: Fold (ASTState TAQ) -> C (ASTState TAQ)
compileFold (Foldl op (ASTState {..})) = do
  id <- mkId
  return $ ASTState id deps $ C.MsgHandler
    { _handleMsg  = handleMsg handlers id src deps
    , _initMsg    = _initMsg handlers
    , _cleanupMsg = _cleanupMsg handlers
    }
  where
    mkId          = C.genId [qc|fold|]
    init          = 0::Int
    handleMsg spec id proj deps msg = do
      topDecl [cdecl|$ty:auto $id:id = $init;|]
      handler <- _handleMsg spec msg
      return $ if msg `elem` deps
        then handler `snoc` [cstm|$id:id = $exp;|]
        else handler
      where
        exp = compileOp op [cexp|$id:id|] [cexp|$id:src|]

compileZip :: Zip (ASTState TAQ) (ASTState TAQ) -> C (ASTState TAQ)
compileZip (ZipWith op ast1 ast2) = do
  id <- mkId
  let ASTState src1 deps1 spec1 = ast1
  let ASTState src2 deps2 spec2 = ast2
  return $ ASTState id (deps1 <> deps2) $ C.MsgHandler
    { _handleMsg  = handleMsg id spec1 spec2 deps1 deps2 src1 src2
    , _initMsg    = _initMsg spec1    -- TODO get both?
    , _cleanupMsg = _cleanupMsg spec1 -- TODO get both?
    }
  where
    fnm              = cnm . _name
    mkId             = C.genId [qc|zip|]
    handleMsg id spec1 spec2 deps1 deps2 proj1 proj2 msg = do
      topDecl [cdecl|$ty:auto $id:id = 0;|]
      handler1 <- (_handleMsg spec1) msg
      handler2 <- (_handleMsg spec2) msg
      let handler = nub $ handler1 ++ handler2 -- will this work?
      return $ if msg `Set.member` (deps1 <> deps2)
        then handler `snoc` [cstm|$id:id = $exp;|]
        else handler
      where
        exp = compileOp op [cexp|$id:proj1|] [cexp|$id:proj2|]

cnm :: String -> String
cnm = rawIden . cname

compileProjection :: C.Specification TAQ
  -> C.MsgHandler TAQ
  -> Projection TAQ
  -> C (ASTState TAQ)
compileProjection (Specification {..}) (MsgHandler {..}) p@(Projection ps _) = do
  id <- C.genId [qc|projection_{pname}|]
  return $ ASTState id (Set.singleton pmsg) $ C.MsgHandler
    { _initMsg    = _initMsg
    , _handleMsg  = handleMsg id
    , _cleanupMsg = _cleanupMsg
    }
  where
    pname = intercalate "_" $ cnm <$> ps
    handleMsg id msg = do
      topDecl =<< declare id <$> mkTy field
      handler <- _handleMsg msg
      return $ if pmsg == msg
        then handler `snoc`
          [cstm|$id:id = msg.$id:(_msgName msg).$id:fieldName;|]
        else handler
    (field, pmsg)  = resolveProjection p
    fieldName      = cnm $ _name field

declare id gty = case gty of
  C.SimpleTy t   -> [cdecl|$ty:t $id:id;|]
  C.ArrayTy t sz -> [cdecl|$ty:t $id:id[$sz];|]

fieldsMap :: Message a -> Map String (Field a)
fieldsMap msg = Map.fromList $ (\field -> (_name field, field)) <$> _fields msg

resolveProjection :: Projection a -> (Field a, Message a)
resolveProjection (Projection [last] (Tip msg)) = (field, msg)
  where
    field = Map.lookup last (fieldsMap msg)
      & fromMaybe
        (error [qc|Field "{last}" not found in message {_msgName msg}|])

resolveProjection (Projection (name:xs) (Node _ m)) = resolveProjection next
  where
    next = Projection xs $ Map.lookup name m
      & fromMaybe (error [qc|"{name}" not found in {Map.keys m}|])

resolveProjection (Projection [] n) = error "Projection too many levels deep."
resolveProjection (Projection xs (Tip {})) = error "Projection too shallow."
resolveProjection _ = error "Internal invariant violated: impossible case."

singleton :: Message a -> Merge a
singleton = Tip
name_ :: Merge a -> String
name_ (Tip msg) = _msgName msg
name_ (Node nm _) = nm
merge :: String -> [Merge a] -> Merge a
merge name = Node name . Map.fromList . map (\m -> (name_ m, m))

mergeStruct :: Merge TAQ -> C C.Type
mergeStruct (Tip msg) = genStruct taqCSpec msg
mergeStruct n@(Node name merges) = do
  membs <- mkMember `mapM` (Map.elems merges)
  cstruct name membs
  where
    mkMember n = do
      struct <- mergeStruct n
      return [csdecl|$ty:struct $id:(name_ n);|]

taqCSpec :: C.Specification TAQ
taqCSpec = C.Specification
  { _mkTy       = mkTy
  , _readMember = readMember
  }

handlerPlain :: C.MsgHandler a
handlerPlain = C.MsgHandler
  { _handleMsg  = handleMsgPlain
  , _initMsg    = initMsgPlain
  , _cleanupMsg = cleanupMsgPlain
  }

-- shamelessly taken from neat-interpolation
dedent :: String -> String
dedent s = case minimumIndent s of
  Just len -> unlines $ map (drop len) $ lines s
  Nothing  -> s
  where
    minimumIndent = listToMaybe . sort . map (length . takeWhile (==' '))
      . filter (not . null . dropWhile isSpace) . lines

noop :: C.Stm
noop = [cstm|/*no-op*/(void)0;|]

initMsgPlain :: Message a -> C [C.Stm]
initMsgPlain = const (pure [noop])

handleMsgPlain :: Message a -> C [C.Stm]
handleMsgPlain = const (pure [noop])

cleanupMsgPlain :: Message a -> C [C.Stm]
cleanupMsgPlain = const (pure [noop])

initMsgGroupBy :: Message TAQ -> C C.Stm
initMsgGroupBy msg = pure noop

handleMsgGroupBy = handleMsgGroupBy__ (C.+=) (getField id "Symbol" taq "trade")

handleMsgGroupBy__ :: (C.Exp -> C.Exp -> C.Stm) -> Field TAQ -> Message TAQ -> C C.Stm
handleMsgGroupBy__ mkStmt field msg = case _msgName msg of
  "trade" -> do
    symbol <- C.simplety <$> mkTy field
    let price = uint
    cppmap <- cpp_unordered_map symbol price
    topDecl [cdecl|$ty:cppmap groupby;|]
    return $ mkStmt [cexp|groupby[msg.trade.symbol]|] [cexp|msg.trade.price|]
  _ -> pure noop

traceWith :: (a -> String) -> a -> a
traceWith f a = unsafePerformIO $ putStrLn (f a) >> return a
trace :: Show a => String -> a -> a
trace s = traceWith $ ((s++": ")++) . show

showc :: Pretty c => c -> String
showc = pretty 0 . ppr

cleanupMsgGroupBy :: Message TAQ -> C C.Stm
cleanupMsgGroupBy msg = case _msgName msg of
  "trade" -> pure [cstm|
    for ($ty:auto it = groupby.begin(); it != groupby.end(); ++it) {
      printf("%.8s %d\n", &it->first, it->second);
    }|]
  _ -> pure noop

auto :: C.Type
auto = [cty|typename $id:("auto")|]

handleMsgGzip :: Message TAQ -> C C.Stm
handleMsgGzip msg = do
  struct <- require $ genStruct taqCSpec msg
  return [cstm|write($id:(fdName msg).writefd,
          &msg.$id:(_msgName msg),
          sizeof(struct $id:(_msgName msg)));|]
  where
    cnm = rawIden . cname . _msgName

cpp_string :: C C.Type
cpp_string = do
  include "string"
  return [cty|typename $id:("std::string")|]

cpp_unordered_map :: C.Type -> C.Type -> C C.Type
cpp_unordered_map k v = do
  include "unordered_map"
  return [cty|typename $id:ty|]
  where
    ty :: String = [qc|std::unordered_map<{showc k}, {showc v}>|]

assert :: Bool -> a -> a
assert pred a = if pred then a else (error "Assertion failed")

symbol :: C C.Type
symbol = do
  include "functional"
  hash <- depends [cedecl|$esc:esc|]
  depends [cty|struct symbol { $ty:ulong symbol; }|]
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
      dep <- cReadIntegral =<< (C.simplety <$> mkTy f)
      return [cstm|dst->$id:cnm = $id:(C.getId dep) ($buf, $len); |]

    cnm     = rawIden (cname nm)

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

program = do
  intermediate1 <- compileAST taqCSpec handlerPlain $
    FoldExp $ Foldl Add $
      ZipExp $ ZipWith Mul
        (ProjectExp $ Projection ["Price"] (Tip tradeFields))
        (ProjectExp $ Projection ["Shares"] (Tip tradeFields))
  intermediate2 <- compileAST taqCSpec handlerPlain $ FoldExp $ Foldl Add
    $ ProjectExp $ Projection ["Price"] (Tip tradeFields)
  cmain taqCSpec (handlers intermediate1) taq

main = do

  shakeArgs shakeOptions $ do

    compileShake False "bin" program

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


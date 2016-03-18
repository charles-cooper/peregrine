{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module Main where

import Protocol.Tmx.TAQ as TAQ
import Protocol
import Protocol.Backend.C.Base as C

import           Language.C.Quote.C
import qualified Language.C.Syntax as C
import qualified Language.C.Smart as C ((+=))
import qualified Language.C.Utils as C
import           Language.C.Utils (C, depends, include, noloc)
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

----------------------------
-- DSL
----------------------------

data Void

data Merge a
  = Tip (Message a)
  | Node String (Map String (Merge a))

data Projection a = Projection
  { _pproto     :: (Proto a)
  , _pmsgName   :: String
  , _pfieldName :: String
  }

data Op = Add | Mul | Div

data Fold a
  = Foldl { foldop :: Op, foldsrc :: a } -- TODO initializer?

data Zip a b
  = ZipWith { zipop :: Op, zipsrca :: a, zipsrcb :: b }
  | OneOf { zipsrca :: a, zipsrcb :: b }

data AST a
  = ZipExp (Zip (AST a) (AST a))
  | GroupByExp (AST a) (Fold (AST a))
  | FoldExp (Fold (AST a))
  | ProjectExp (Projection a)
  | ObserveExp (Maybe String) (AST a)
  | ConstExp Constant

data Constant
  = ConstInt Int
  | ConstDouble Double

set :: Ord a => [a] -> Set a
set = Set.fromList

compileOp :: Op -> (C.Exp -> C.Exp -> C.Exp)
compileOp Add = (+)
compileOp Mul = (*)
compileOp Div = (/)

compileAST :: Ord p
  => C.Specification p
  -> C.MsgHandler p
  -> AST p
  -> C (ASTState p)
compileAST spec handler ast = case ast of
  FoldExp (Foldl op ast) -> do
    st <- compileAST spec handler ast
    compileFold (Foldl op st)
  ZipExp z -> case z of
    (ZipWith op ast1 ast2) -> do
      st1 <- compileAST spec handler ast1
      st2 <- compileAST spec handler ast2
      compileZip (ZipWith op st1 st2)
    (OneOf ast1 ast2) -> do
      st1 <- compileAST spec handler ast1
      st2 <- compileAST spec handler ast2
      compileZip (OneOf st1 st2)
  GroupByExp a (Foldl op ast) -> do
    st1 <- compileAST spec handler a
    st2 <- compileAST spec handler ast
    compileGroupBy st1 (Foldl op st2)
  ProjectExp p -> compileProjection spec handler p
  ObserveExp mdesc ast -> do
    st <- compileAST spec handler ast
    compileObservation mdesc spec ast st
  ConstExp int -> compileConst int

data ASTState p = ASTState
  { src      :: String
  , deps     :: Set (Message p)
  , handlers :: [C.MsgHandler p]
  }

auto :: C.Type
auto = [cty|typename $id:("auto")|]

compileConst :: Constant -> C (ASTState p)
compileConst constant = do
  id <- C.genId "constant"
  return $ ASTState id (Set.empty) $ pure $ C.MsgHandler
    { _handleMsg  = init id
    , _initMsg    = mempty_
    , _cleanupMsg = mempty_
    }
  where
    init id _ = do
      topDecl [cdecl|/*const*/ $ty:ty $id:id = $value;|]
      return []
    value = case constant of
      ConstInt int -> [cexp|$int|]
      ConstDouble d -> [cexp|$d|]
    ty = case constant of
      ConstInt _    -> ulong
      ConstDouble _ -> [cty|double|]

compileObservation :: Ord p
  => Maybe String
  -> C.Specification p
  -> AST p
  -> ASTState p
  -> C (ASTState p)
compileObservation mdesc spec ast (ASTState src deps handlers) = do
  return $ ASTState src deps $ snoc handlers $ C.MsgHandler
    { _handleMsg  = mempty_
    , _initMsg    = mempty_
    , _cleanupMsg = printer
    }
  where
    -- busted busted busted
    printer msg = return $ if msg == head (_outgoingMessages (_proto spec))
      then case ast of
        GroupByExp {} -> [cstms|
          printf($desc);
          for ($ty:auto it = $id:src.begin(); it != $id:src.end(); ++it) {
            printf("  %.8s: %d\n", &it->first, it->second);
          }
        |]
        FoldExp {} -> [cstms|
          printf($(desc ++ "%d\n"), $id:src);
        |]
      else mempty
    desc = maybe "" (++": ") mdesc
    inferType ast = case ast of
      ObserveExp _ a           -> inferType a
      FoldExp (Foldl _ a)      -> inferType a
      ZipExp (OneOf a b)       -> inferType a -- assert equals?
      ZipExp (ZipWith _ a b)   -> inferType a -- assert equals?
      ProjectExp p             -> do
        t <- _mkTy spec $ fst $ resolveProjection p
        return $ Simple t
      GroupByExp k (Foldl _ v) -> CPPMap <$> (inferType k) <*> (inferType v)

data GType2 = Simple C.GType | CPPMap GType2 GType2

compileGroupBy :: Ord a => ASTState a -> Fold (ASTState a) -> C (ASTState a)
compileGroupBy (ASTState key deps1 handlers1) (Foldl op (ASTState value deps2 handlers2)) = do
  id <- mkId
  return $ ASTState id deps $ snoc (handlers1<>handlers2) $ C.MsgHandler
    { _handleMsg  = handleMsg id deps
    , _initMsg    = mempty_
    , _cleanupMsg = mempty_
    }
  where
    deps = deps1 <> deps2
    mkId  = C.genId [qc|group_by|]
    mapTy = cpp_unordered_map (decltype key) (decltype value)
    handleMsg id deps msg = do
      mapty <- mapTy
      topDecl [cdecl|$ty:mapty $id:id;|]
      if (msg `Set.member` deps)
        then return [cstms|
          if (! $id:id.count($id:key)) {
            $id:id[$id:key] = $init;
          }
          $id:id[$id:key] = $exp;
          |]
        else return []
      where
        exp  = compileOp op [cexp|$id:id[$id:key]|] [cexp|$id:value|]
        init = 0::Int

decltype :: String -> C.Type
decltype id = [cty|typename $id:("decltype("<>id<>")")|]

compileFold :: Ord a => Fold (ASTState a) -> C (ASTState a)
compileFold (Foldl op (ASTState {..})) = do
  id <- mkId
  return $ ASTState id deps $ snoc handlers $ C.MsgHandler
    { _handleMsg  = handleMsg id
    , _initMsg    = mempty_
    , _cleanupMsg = mempty_
    }
  where
    mkId          = C.genId [qc|fold|]
    init          = 0::Int
    handleMsg id msg = do
      topDecl [cdecl|$ty:(decltype src) $id:id;|]
      return $ if msg `elem` deps
        then [[cstm|$id:id = $exp;|]]
        else []
      where
        exp = compileOp op [cexp|$id:id|] [cexp|$id:src|]

compileZip :: Ord a => Zip (ASTState a) (ASTState a) -> C (ASTState a)
compileZip z = do
  id <- mkId
  let ASTState src1 deps1 spec1 = ast1
  let ASTState src2 deps2 spec2 = ast2
  return $ ASTState id (deps1 <> deps2) $ snoc (spec1<>spec2) $ C.MsgHandler
    { _handleMsg  = handleMsg id deps1 deps2 src1 src2
    , _initMsg    = mempty_
    , _cleanupMsg = mempty_
    }
  where
    (ast1, ast2)     = (zipsrca z, zipsrcb z)
    fnm              = cnm . _name
    mkId             = C.genId [qc|zip|]
    handleMsg id deps1 deps2 proj1 proj2 msg = do
      topDecl [cdecl|$ty:ty $id:id;|]
      -- ^ TODO don't need this for the temporary OneOf variable
      return $ if msg `Set.member` (deps1 <> deps2)
      -- ^ TODO abstract this out somehow.
        then [[cstm|$id:id = $exp;|]]
        else []
      where
        ty  = case z of
          (ZipWith op _ _) -> decltype (showc exp)
          (OneOf {})       -> decltype (proj1)
        exp = case z of
          (ZipWith op _ _) -> compileOp op [cexp|$id:proj1|] [cexp|$id:proj2|]
          (OneOf {}) -> let
            p1 = msg `Set.member` deps1
            p2 = msg `Set.member` deps2
            in if p1 && not p2
              then [cexp|$id:proj1|]
              else if not p1 && p2
                then [cexp|$id:proj2|]
                else if p1 && p2
                  then error [qc|Impossible merge.|]
                  else error [qc|Internal invariant violated __FILE__:__LINE__|]

cnm :: String -> String
cnm = rawIden . cname

compileProjection :: Ord a
  => C.Specification a
  -> C.MsgHandler a
  -> Projection a
  -> C (ASTState a)
compileProjection (Specification {..}) (MsgHandler {..}) p = do
  id <- C.genId pname
  return $ ASTState id (Set.singleton pmsg) $ pure $ C.MsgHandler
    { _initMsg    = mempty_
    , _handleMsg  = handleMsg id
    , _cleanupMsg = mempty_
    }
  where
    pname = intercalate "_" $ cnm <$> [_namespace proto, m, f]
      where (Projection proto m f) = p
    handleMsg id msg = do
      topDecl =<< declare id <$> _mkTy field
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
msgsMap :: Proto a -> Map String (Message a)
msgsMap proto = Map.fromList $ (\msg -> (_msgName msg, msg)) <$> _outgoingMessages proto

resolveProjection :: Projection a -> (Field a, Message a)
resolveProjection (Projection proto msgName fieldName) = (field, msg)
  where
    msg = Map.lookup msgName (msgsMap proto)
      & fromMaybe
        (error [qc|Message "{msgName}" not found in proto {_namespace proto}|])
    field = Map.lookup fieldName (fieldsMap msg)
      & fromMaybe 
        (error [qc|Field "{fieldName}" not found in message {_msgName msg}|])

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

------------------------------------
-- TMX TAQ
------------------------------------

taqCSpec :: C.Specification TAQ
taqCSpec = C.Specification
  { _proto      = taq
  , _mkTy       = mkTy
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
initMsgPlain = const (pure [])

handleMsgPlain :: Message a -> C [C.Stm]
handleMsgPlain = const (pure [])

cleanupMsgPlain :: Message a -> C [C.Stm]
cleanupMsgPlain = const (pure [])

traceWith :: (a -> String) -> a -> a
traceWith f a = unsafePerformIO $ putStrLn (f a) >> return a
trace :: Show a => String -> a -> a
trace s = traceWith $ ((s++": ")++) . show

showc :: Pretty c => c -> String
showc = pretty 0 . ppr

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

-- x = sum(trade.price * quote.bid_size)
-- y = quote.ask_size
-- x + y
intermediate :: AST TAQ
intermediate = observe $ GroupByExp symbol $ Foldl Add trade_share
  where
    observe     = ObserveExp $ Just "sum(price*shares)"
    symbol      = ZipExp $ OneOf
      (ProjectExp $ Projection taq "trade" "Symbol")
      (ProjectExp $ Projection taq "quote" "Symbol")
    trade_price = ProjectExp $ Projection taq "trade" "Price"
    trade_share = ProjectExp $ Projection taq "trade" "Shares"
    trade_value = ZipExp $ ZipWith Mul trade_price trade_share
    ask_size    = ProjectExp $ Projection taq "quote" "Ask Size"
    bid_size    = ProjectExp $ Projection taq "quote" "Bid Size"

{-
vwap :: AST TAQ
vwap = ObserveExp (Just "vwap") vwap
  where
    -- multiply by 1.0 to force cast
    price  = ZipExp $ ZipWith Mul
      (ConstExp (ConstDouble 1.0))
      (ProjectExp $ Projection taq "trade" "Price")
    shares = ProjectExp $ Projection taq "trade" "Shares"
    symbol = ProjectExp $ Projection taq "trade" "Symbol"
    vwap   = GroupByExp symbol $ Foldl Add $
      ZipExp $ ZipWith Div
        (FoldExp $ Foldl Add (ZipExp $ ZipWith Mul price shares))
        (FoldExp $ Foldl Add (ConstExp (ConstDouble 1.0)))
-}

program = do
  intermediate1 <- compileAST taqCSpec handlerPlain intermediate
  intermediate2 <- compileAST taqCSpec handlerPlain $ ObserveExp (Just "Sum price") $ FoldExp $ Foldl Add
    $ ProjectExp $ Projection taq "trade" "Price"
  intermediate3 <- compileAST taqCSpec handlerPlain $
    ObserveExp (Just "count trades") $ GroupByExp
      (ProjectExp $ Projection taq "trade" "Symbol")
      (Foldl Add (ConstExp (ConstInt 1)))
  -- intermediate4 <- compileAST taqCSpec handlerPlain vwap

  cmain taqCSpec (mconcat . mconcat $ handlers <$> [intermediate1, intermediate2, intermediate3])

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

    want ["Run a.out"]


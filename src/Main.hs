{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module Main where

import Protocol
import Protocol.Backend.C.Base as C
import Protocol.Tmx.TAQ as TAQ
import Protocol.Tmx.TAQ.C as TAQ

import Protocol.Nasdaq.ITCH.Proto as ITCH
import Protocol.Nasdaq.ITCH.Proto.C as ITCH

import           Language.C.Quote.C
import qualified Language.C.Syntax as C
import qualified Language.C.Smart as C ((+=))
import qualified Language.C.Utils as C
import           Language.C.Utils (C, depends, include, noloc)
import           Language.C.Utils (topDecl, require)
import           Language.C.Utils (char, bool, ushort, uint, ulong)
import           Language.C.Lib

import Data.Monoid
import Data.List (intercalate)
import Data.Maybe

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
          [cstm|$id:id = msg.$id:(cnm $ _msgName msg).$id:fieldName;|]
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

mergeStruct :: C.Specification a -> Merge a -> C C.Type
mergeStruct spec (Tip msg) = genStruct spec msg
mergeStruct spec n@(Node name merges) = do
  membs <- mkMember `mapM` (Map.elems merges)
  cstruct name membs
  where
    mkMember n = do
      struct <- mergeStruct spec n
      return [csdecl|$ty:struct $id:(name_ n);|]

handlerPlain :: C.MsgHandler a
handlerPlain = C.MsgHandler
  { _handleMsg  = handleMsgPlain
  , _initMsg    = initMsgPlain
  , _cleanupMsg = cleanupMsgPlain
  }

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

itchProgram = do
  intermediate <- compileAST itchCSpec handlerPlain $ ProjectExp $ Projection itch "Order Executed With Price" "Executed Shares"
  cmain itchCSpec (mconcat $ handlers intermediate)

main = do

  shakeArgs shakeOptions $ do

    compileShake False "bin" "runTmx" program
    compileShake False "bin" "runItch" itchProgram

    let exe = "bin/runTmx"

    "data/*.lz4" %> \out -> do
      let src = "/mnt/efs/ftp/tmxdatalinx.com" </> (takeFileName out) -<.> "gz"
      need [src]
      command [Shell] [qc|gzip -d < {src} | lz4 -9 > {out}|] []

    let date = "20150826"
    phony "Run runTmx" $ do
      let dataFile = [qc|data/{date}TradesAndQuotesDaily.lz4|]
      need [dataFile, exe]
      command_ [Shell] [qc|lz4 -d < {dataFile} | {exe} {date} |] []

    want ["bin/runItch"]
    want ["bin/runTmx"]


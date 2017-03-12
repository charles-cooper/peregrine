{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Peregrine.DSL where

import           Language.Peregrine.AST

import           Control.Monad.Reader
import           Data.Fix

import           Protocol
import           Protocol.Backend.C.Base

import           Data.Ratio

type Signal = Fix (ASTF (Context Group))
   
newtype Group = Group { getGroup :: [Signal] }
  deriving (Eq, Ord)

-- Monad for Peregrine language. It keeps knowledge of groupBy fences
type Peregrine = Reader Group Signal

incomplete :: a
incomplete = error "Incomplete instance"
instance Num Signal where
  fromInteger = Fix . ConstExp . ConstInt . fromInteger
  (+)    = incomplete
  (*)    = incomplete
  (-)    = incomplete
  abs    = incomplete
  signum = incomplete
      
instance Num Peregrine where
  fromInteger = return . fromInteger
  a + b       = join $ (+.) <$> a <*> b
  a * b       = join $ (*.) <$> a <*> b
  a - b       = join $ (-.) <$> a <*> b
  abs         = incomplete
  signum      = incomplete

instance Fractional Peregrine where
  fromRational x = fromIntegral (numerator x) / fromInteger (denominator x)
  x / y          = join $ (/.) <$> x <*> y
      
addAnnotation :: String -> ASTF (Context a) b -> ASTF (Context a) b
addAnnotation s = mapCtx setAnn
  where
    setAnn (Context x _) = Context x (Just s)

-- Set the C variable name programmatically
infixr 1 @!
(@!) :: Peregrine -> String -> Peregrine
p @! ann = do
  sig <- p
  return $ Fix (addAnnotation ann (unFix sig))

groupBy :: Signal -> Peregrine -> Peregrine
groupBy group next = do
  local (Group . (group:) . getGroup) next
      
signal :: (Context Group -> Signal) -> Peregrine
signal ast = do
  gs <- ask
  ctx <- pure $ Context gs Nothing
  return (ast ctx)
      
merge :: Signal -> Signal -> Peregrine
merge x y = signal $ \ctx -> Fix $ MergeExp ctx (x) (y)
      
project :: Proto CField -> String -> String -> Peregrine
project p x y = signal $ \ctx -> Fix $ ProjectExp ctx (Projection p x y)
      
zipWithP :: BinOp -> Signal -> Signal -> Peregrine
zipWithP op x y = signal $ \ctx -> Fix $ ZipWith ctx op (x) (y)
      
foldP :: BinOp -> Signal -> Peregrine
foldP op x = signal $ \ctx -> Fix $ FoldExp ctx op (x)
      
mapP :: UnaryOp -> Signal -> Peregrine
mapP f x = signal $ \ctx -> Fix $ MapExp ctx f x
      
guardP :: Signal -> Signal -> Peregrine
guardP pred x = signal $ \ctx -> Fix $ GuardExp ctx (pred) (x)
      
fireWhen :: Signal -> Signal -> Peregrine
fireWhen x y = signal $ \ctx -> Fix $ RestrictExp ctx x y
      
window :: BinOp -> Int -> Signal -> Signal -> Peregrine
window op span t x = signal $ \ctx -> Fix $ WindowExp ctx op span t x
      
lastP :: Signal -> Peregrine
lastP x = signal $ \ctx -> Fix $ LastExp ctx (x)
      
-- TODO change the group, `observe x` should have same group as `x`
observe :: Signal -> Peregrine
observe x = signal $ \ctx -> Fix $ ObserveExp ctx Every x
      
summary :: Signal -> Peregrine
summary x = signal $ \ctx -> Fix $ ObserveExp ctx Summary x

infixl 8 ==.
infixl 7 <.
infixl 7 >.
infixl 7 <=.
infixl 7 >=.
infixl 6 +.
infixl 6 -.
infixl 5 /.
infixl 5 *.
     
(==.) :: Signal -> Signal -> Peregrine
(==.) = zipWithP Eq
     
(>.) :: Signal -> Signal -> Peregrine
(>.) = zipWithP Gt
     
(>=.) :: Signal -> Signal -> Peregrine
(>=.) = zipWithP Ge
     
(<.) :: Signal -> Signal -> Peregrine
(<.) = zipWithP Lt
     
(<=.) :: Signal -> Signal -> Peregrine
(<=.) = zipWithP Le
      
(+.) :: Signal -> Signal -> Peregrine
(+.) = zipWithP Add
      
(-.) :: Signal -> Signal -> Peregrine
(-.) = zipWithP Sub
      
(/.) :: Signal -> Signal -> Peregrine
(/.) = zipWithP Div
      
(*.) :: Signal -> Signal -> Peregrine
(*.) = zipWithP Mul

-- LIBRARY
-- As this section grows larger it could go into its own 'DSL.Lib' module
diff :: Signal -> Peregrine
diff sig = do
  sig' <- lastP sig
  sig -. sig' @! "diff"
 
sumGroupBy :: Signal -> Peregrine -> Peregrine
sumGroupBy group sig = do
  grouped <- groupBy group $ do
    s <- sig
    diff s
  sumP grouped

sumP :: Signal -> Peregrine
sumP xs = foldP Add xs
 
meanP :: Signal -> Peregrine
meanP xs = do
  len <- countP xs
  len <- join $ guardP <$> len >. 0 <*> pure len
  sumP xs / pure len

countP :: Signal -> Peregrine
countP sig = (@! "count") $ do
  sumP =<< 1 `fireWhen` sig
 
sqrtP :: Signal -> Peregrine
sqrtP = mapP (Math "sqrt")
 
absP :: Signal -> Peregrine
absP = mapP (Math "abs")

covariance :: Signal -> Signal -> Peregrine
covariance x y = (@! "covariance") $ do
  cross <- sumP =<< (x *. y)                  @! "cross"
  sumx  <- sumP x                             @! "sumx"
  sumy  <- sumP y                             @! "sumy"
  len   <- countP cross                       @! "rawlen"
  pred  <- len >. 1
  len   <- guardP pred len                    @! "len"
  (pure cross - (pure sumx * pure sumy / pure len)) / (pure len - 1)
 
variance :: Signal -> Peregrine
variance x = do
  sumSq <- sumP =<< x *. x
  sum   <- sumP x
  len   <- countP x
  pred  <- len >. 1
  len   <- guardP pred len
  (pure sumSq - (pure sum * pure sum / pure len)) / (pure len - 1)
 
correlation :: Signal -> Signal -> Peregrine
correlation x y = (@! "correlation") $ do
  cross <- sumP =<< (x *. y)    @! "Cross"
  sumx  <- sumP x               @! "sum x"
  sumx2 <- sumP =<< (x *. x)    @! "sum x^2"
  sumy  <- sumP y               @! "sum y"
  sumy2 <- sumP =<< (y *. y)    @! "sum y^2"
  len   <- countP cross         @! "raw len"
  pred  <- len >. 1
  len   <- guardP pred len      @! "len"
  cov  <- (pure cross - (pure sumx * pure sumy / pure len)) / (pure len - 1)
  varx <- (pure sumx2 - (pure sumx * pure sumx / pure len)) / (pure len - 1)
  vary <- (pure sumy2 - (pure sumy * pure sumy / pure len)) / (pure len - 1)
  pure cov / (sqrtP =<< pure varx * pure vary)


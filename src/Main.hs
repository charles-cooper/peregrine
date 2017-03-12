module Main where

import           Language.Peregrine
import           Protocol.Tmx.TAQ.C as TAQ

import           Control.Monad

symbolP :: Peregrine
symbolP = do
  tsymbol <- project ctaq "trade" "Symbol" @! "tsymbol"
  qsymbol <- project ctaq "quote" "Symbol" @! "qsymbol"
  merge tsymbol qsymbol                    @! "symbol"

vwapP :: Signal -> Peregrine
vwapP group = groupBy group $ do
  px <- taqTradePrice
  sz <- taqTradeSize
  value  <- sumP =<< px *. sz
  volume <- sumP sz
  value /. volume

midpointP :: Signal -> Peregrine
midpointP group = groupBy group $ do
  x   <- taqBidPrice + taqAskPrice
  y   <- x /. 2
  return y                               @! "midpoint"

weightedMidpointP :: Signal -> Peregrine
weightedMidpointP group = groupBy group $ do
  bidpx  <- taqBidPrice                     @! "bidpx" -- test CSE working
  bidsz  <- taqBidSize
  askpx  <- taqAskPrice
  asksz  <- taqAskSize
  bidval <- bidpx  *. bidsz                 @! "bidval"
  askval <- askpx  *. asksz                 @! "askval"
  totsz  <- bidsz  +. asksz                 @! "totsz"
  tmpsum <- bidval +. askval                @! "tmp"
  pred   <- totsz >. 0
  ret    <- tmpsum /. totsz
  guardP pred ret                           @! "weighted midpoint"

midpointSkew :: Signal -> Peregrine
midpointSkew group = do
  normalMid <- midpointP group         @! "midpoint"
  weightMid <- weightedMidpointP group @! "weighted midpoint"
  groupBy group $ do
    weightMid -. normalMid             @! "midpoint skew"

-- Nonce program to use all the AST types
simpleProgram :: Signal -> Peregrine
simpleProgram group = do
  midp   <- midpointP group
  symbol <- symbolP
  groupBy symbol $ do
    bid  <- taqBidPrice
    lbid <- lastP bid                       @! "lastbid"
    sum  <- sumP lbid                       -- @! "sumbid"
    pred <- sum >. 0                        @! "pred"
    twice_sum <- sum +. sum                 @! "twice sum"
    x <- guardP pred sum                    @! "guarded sum"
    taqTradePrice * (x /. midp)             @! "weird quantity"

-- This program represents the sharing problem because multiple nodes refer to
-- the `bid` node and so it will get compiled twice unless we modulo
-- the sharing
problem :: Peregrine
problem = do
  bid <- taqBidPrice
  y <- bid +. bid
  bid +. y

zipBug :: Peregrine
zipBug = do
  s <- project ctaq "quote" "Symbol"
  groupBy s $ do
    bid <- taqBidPrice
    ask <- taqAskPrice
    x   <- bid +. ask                      @! "x"
    y   <- bid -. ask                      @! "y"
    x +. y                                 @! "bug"

tradeField :: String -> Peregrine -- helper func
tradeField field = project ctaq "trade" field @! field

taqTradePrice :: Peregrine
taqTradePrice = tradeField "Price" / 1000

taqTradeSize :: Peregrine
taqTradeSize = tradeField "Shares"

taqTradeTime :: Peregrine
taqTradeTime = tradeField "Trade Time"

quoteField :: String -> Peregrine -- helper func
quoteField field = project ctaq "quote" field @! field

taqBidPrice :: Peregrine
taqBidPrice = quoteField "Bid Price" / 1000

taqBidSize :: Peregrine
taqBidSize = quoteField "Bid Size"

taqAskPrice :: Peregrine
taqAskPrice = quoteField "Ask Price" / 1000

taqAskSize :: Peregrine
taqAskSize = quoteField "Ask Size"

marketBidVal :: Peregrine
marketBidVal = (@! "Market Bid Val") $ do
  s <- symbolP
  sumGroupBy s $ do
    bidpx <- project ctaq "quote" "Bid Price" @! "bid price"
    bidsz <- project ctaq "quote" "Bid Size"  @! "Bid size"
    bidpx *. bidsz                           @! "bid val"

groupBySymbol :: (Signal -> Peregrine) -> Peregrine
groupBySymbol signalWithGroup = do
  symbol <- symbolP
  signalWithGroup symbol

main :: IO ()
main = do
  runDirectory "data/TAQ/" TAQ.cspec $ do
    s <- symbolP
    mps <- midpointSkew s
    mp  <- midpointP s
    wmp <- weightedMidpointP s
    vwap <- vwapP s
    groupBy s $ do
      spread    <- taqAskPrice - taqBidPrice   @! "spread"
      avgSpread <- meanP spread                @! "avg spread"
      ret <- 100 * (avgSpread /. vwap)         @! "avg spread / vwap"

      t   <- taqTradeTime
      px  <- taqTradePrice
      maxWin <- window Max 1000 t px           @! "max window"
      minWin <- window Min 1000 t px           @! "min window"
      let accum = mapP (Window "accumulate")
      spike  <- accum maxWin - accum minWin    @! "spike"
      ret <- foldP Max spike                   @! "max spike over day"
      summary ret


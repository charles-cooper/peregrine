{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
-- https://api.tmxwebstore.com/commerce/product/docs/view/trades-and-quotes-specifications-2015-11-16-en.pdf
-- From https://www.tmxwebstore.com/products/trades-and-quotes-daily#view-details, accessed 01/09/2018

module Protocol.Tmx.TAQ (
  tradeFields,
  quoteFields,
  TAQ(..),
  taq,
) where

import Protocol
import Text.InterpolatedString.Perl6 (qc)

import Data.Text as T (Text, unpack)

data TAQ = Numeric | Alphabetic | Date | Time | Boolean
  deriving (Eq, Show, Ord)
taq = Proto
  { _namespace         = "taq"
  , _lineSeparated     = True
  , _pktHdrLen         = 0
  , _outgoingMessages  = [dateRecord, tradeFields, quoteFields]
  , _incomingMessages  = []
  }
field :: Int -> Int -> String -> TAQ -> Text -> Field TAQ
field pos {- 1 based -} len name ty note =
  Field (pos - 1 - 1) len name ty (T.unpack note)

dateRecord :: Message TAQ
dateRecord = Message "date" 'D'
  [ field 2 8 "Date" Date [note|YYYYMMDD|]
  ]
tradeFields :: Message TAQ
tradeFields = Message "trade" 'T'
  [ field 2 8 "Symbol" Alphabetic [note|Alphabetic|]

  , field 10 8 "Trade Time" Time
    [note|
      HHMMSS00 (last 2 digits are "00")
      Manual Trade Time.  Future enhancement may include micro-seconds.
    |]

  , field 18 9 "Sequence Number" Numeric
    [note|
    Numeric: Sequences trades and quotes records across all stocks.
    |]

  , field 27 7 "Price" Numeric [qc|$$$$CCC|]
  , field 34 9 "Shares" Numeric [note|Numeric|]
  , field 43 3 "Buyer" Numeric [note|Numeric|]
  , field 46 3 "Seller" Numeric [note|Numeric|]
  , field 49 1 "Odd Lot" Boolean [note|Marker: On = "1", Off = blank|]

  , field 50 1 "Trade Session" Alphabetic
    [note|
    Alphabetic:
      "A" - Continuous Market;
      "O" - Open Trade;
      "M" - Market On Close Trade;
      "C" - Crossing Session
    |]

  , field 51 1 "Cancellation" Boolean
    [note|
    Marker: On = "1" Off = blank
    Cancellation record appearing at the time the trade is cancelled.
    |]

  , field 52 1 "Cancelled" Boolean
    [note|
    Marker: On = "1", Off = blank
    Marker on original trade to indicate the trade has been cancelled.
    |]

  , field 53 1 "Correction" Boolean
    [note|Marker: On = "1", Off = blank|]

  , field 54 1 "Delayed Delivery" Boolean
    [note|Marker: On = "1", Off = blank|]

  , field 55 1 "Cash" Boolean
    [note|Marker: On = "1", Off = blank|]

  , field 56 1 "Non Net" Boolean
    [note|Marker: On = "1", Off = blank|]

  , field 57 1 "Special Terms" Boolean
    [note|Marker: On = "1", Off = blank|]

  , field 58 1 "Specialty Cross" Alphabetic
    [note|
    Alphabetic:
      "B" - Basis Cross;
      "V" - VWAP Cross;
      "C" - Contingent Cross;
      "I" - Internal Cross;
      "S" - Special Trading Session Cross
    |]
  ]

quoteFields :: Message TAQ
quoteFields = Message "quote" 'Q'
  [ field 2 8 "Symbol" Alphabetic
    [note|Alphabetic|]

  , field 10 8 "Time" Time
    [note|
      HHMMSS00 (last 2 digits are "00")
      Manual Trade Time
    |]

  , field 18 9 "Sequence Number" Numeric
    [note|
      Numeric: Sequences trades and quotes records across all stocks.
    |]

  , field 27 7 "Bid Price" Numeric [qc|$$$$CCC|]
  , field 34 7 "Ask Price" Numeric [qc|$$$$CCC|]
  , field 41 3 "Bid Size" Numeric [note|Numeric, In Boardlots|]
  , field 44 3 "Ask Size" Numeric [note|Numeric, In Boardlots|]
  , field 47 1 "Halted" Boolean [note|Marker: On = "1", Off = blank|]
  ]


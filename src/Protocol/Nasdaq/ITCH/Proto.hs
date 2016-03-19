-- http://www.nasdaqtrader.com/content/technicalsupport/specifications/dataproducts/NQTVITCHSpecification.pdf
-- VERSION 5.0 03/06/2015
{-# LANGUAGE QuasiQuotes #-}
module Protocol.Nasdaq.ITCH.Proto where

import Protocol
import qualified Data.Text as T

data ITCH = Integer | Price4 | Price8 | Alpha
  deriving (Eq, Show, Ord)
field :: String -> Int -> Int -> ITCH -> T.Text -> Field ITCH
field name ofst len ty notes = Field (ofst-1) len name ty (T.unpack notes)
itch = Proto
  { _namespace         = "itch"
  , _lineSeparated     = False
  , _pktHdrLen         = 2
  , _incomingMessages  = []
  , _outgoingMessages  =
    [ Message "System Event Message" 'S'
      [ field "Stock Locate" 1 2 Integer [note|Always 0|]
      , field "Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Nanoseconds since midnight.|]
      , field "Event Code" 11 1 Alpha [note|See System Event Codes below.|]
      ]
    , Message "Stock Directory" 'R'
      [ field "Stock Locate" 1 2 Integer
        [note|Locate Code uniquely assigned to the security symbol for the day.|]
      , field "Tracking Number" 3 2 Integer
        [note|NASDAQ OMX internal tracking number|]

      , field "Timestamp" 5 6 Integer
        [note|
          Time at which the directory message was generated. Refer to Data Types for field processing notes.
        |]

      , field "Stock" 11 8 Alpha
        [note|
          Denotes the security symbol for the issue in the NASDAQ execution system.
        |]

      , field "Market Category" 19 1 Alpha
        [note|
          Indicates Listing market or listing market tier for the issue
          Code Definition
            NASDAQ-Listed Instruments
              Q NASDAQ Global Select Market
              G NASDAQ Global Market
              S NASDAQ Capital Market
            Non-NASDAQ-Listed Instruments
              N New York Stock Exchange (NYSE)
              A NYSE MKT
              P NYSE Arca
              Z BATS Z Exchange
              <space> Not available
        |]

      , field "Financial Status Indicator" 20 1 Alpha
        [note|
        For NASDAQ-listed issues, this field indicates when a firm is not in compliance with NASDAQ continued listing requirements.
        Code Definition
          NASDAQ-Listed Instruments
            D Deficient
            E Delinquent
            Q Bankrupt
            S Suspended
            G Deficient and Bankrupt
            H Deficient and Delinquent
            J Delinquent and Bankrupt
            K Deficient, Delinquent and Bankrupt
            C Creations and/or Redemptions Suspended for Exchange Traded Product
            N Normal (Default): Issuer Is NOT Deficient, Delinquent, or Bankrupt
          Non-NASDAQ-Listed Instruments
            <space> Not available. Firms should refer to SIAC feeds for code if needed. 
        |]

      , field "Round Lot Size" 21 4 Integer
        [note|Denotes the number of shares that represent a round lot for the issue|]
      , field "Round Lots Only" 25 1 Alpha
        [note|Indicates if NASDAQ system limits order entry for issue
          Code Definition
          Y NASDAQ OMX system only accepts round lots orders for this security.
          N NASDAQ OMX system does not have any order size restrictions for this security. Odd and mixed lot orders are allowed.
        |]

      , field "Issue Classification" 26 1 Alpha
        [note|Identifies the security class for the issue as assigned by NASDAQ. See Appendix for allowable values.|]

      , field "Issue Sub-Type" 27 2 Alpha
        [note|Identifies the security sub-type for the issue as assigned by NASDAQ. See Appendix for allowable values.|]

      , field "Authenticity" 29 1 Alpha
        [note|Denotes if an issue or quoting participant record is set-up in NASDAQ systems in a live/production, test, or demo state. Please note that firms should only show live issues and quoting participants on public quotation displays.
        Code Definition
          P Live/Production
          T Test
        |]
      , field "Short Sale Threshold Indicator" 30 1 Alpha
        [note|Indicates if a security is subject to mandatory close-out of short sales under SEC Rule 203(b)(3).
        Code Definition
          Y Issue is restricted under SEC Rule 203(b)(3)
          N Issue is not restricted
          <space> Threshold Indicator not available
        |]
      , field "IPO Flag" 31 1 Alpha
        [note|Indicates if the NASDAQ security is set up for IPO release. This field is intended to help NASDAQ market participant firms comply with FINRA Rule 5131(b).
        Code Definition
          NASDAQ-Listed Instruments
            Y NASDAQ listed instrument is set up as a new IPO security
            N NASDAQ listed instrument is not set up as a new IPO security
          Non-NASDAQ-Listed Instruments
            <space> Not available
        |]
      , field "LULD Reference Price Tier" 32 1 Alpha
        [note|Indicates which Limit Up / Limit Down price band calculation parameter is to be used for the instrument. Refer to LULD Rule for details.
        Code Definition
          1 Tier 1 NMS Stocks and select ETPs
          2 Tier 2 NMS Stocks
          <space> Not applicable
        |]
      , field "ETP Flag" 33 1 Alpha
        [note|Indicates whether the security is an exchange traded product (ETP):
          Code Definition
            Y Instrument is an ETP
            N Instrument is not an ETP
            <space> Not available
        |]

      , field "ETP Leverage Factor" 34 4 Integer
        [note|Tracks the integral relationship of the ETP to the underlying index. Example: If the underlying Index increases by a value of 1 and the ETP’s Leverage factor is 3, indicates the ETF will increase/decrease (see Inverse) by 3.
          Note: Leverage Factor of 1 indicates the ETP is NOT leveraged.
          This field is used for LULD Tier I price band calculation purposes.
        |]

      , field "Inverse Indicator" 38 1 Alpha
        [note|Indicates the directional relationship between the ETP and underlying index.
          Code Definition
            Y ETP is an Inverse ETP
            N ETP is not an Inverse ETP
          Example: An ETP Leverage Factor of 3 and an Inverse value of ‘Y’ indicates the ETP will decrease by a value of 3.
        |]
      ]
    , Message "Stock Trading Action" 'H'
      [ field "Stock Locate" 1 2 Integer
        [note|Locate code identifying the security|]
      , field "Tracking Number" 3 2 Integer
        [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer
        [note|Nanoseconds since midnight|]
      , field "Stock" 11 8 Alpha [note|Stock symbol, right padded with spaces|]
      , field "Trading State" 19 1 Alpha
        [note|Indicates the current trading state for the stock.
          Allowable values:
            “H” = Halted across all U.S. equity markets / SROs
            “P” = Paused across all U.S. equity markets / SROs (NASDAQ-listed securities only)
            “Q” = Quotation only period for cross-SRO halt or pause
            “T” = Trading on NASDAQ
        |]
       , field "Reserved" 20 1 Alpha [note|Reserved.|]
       , field "Reason" 21 4 Alpha [note|Trading Action reason.|]
      ]
    , Message "Reg SHO Restriction" 'Y'
      [ field "Locate Code" 1 2 Integer
        [note|Locate code identifying the security|]

      , field "Tracking Number" 3 2 Integer
        [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Nanoseconds since midnight|]
      , field "Stock" 11 8 Alpha [note|Stock symbol, right padded with spaces|]
      , field "Reg SHO Action" 19 1 Alpha
        [note|Denotes the Reg SHO Short Sale Price Test Restriction status for the issue at the time of the message dissemination. Allowable values are:
          “0” = No price test in place
          “1” = Reg SHO Short Sale Price Test Restriction in effect due to an intra-day price drop in security
          “2” = Reg SHO Short Sale Price Test Restriction remains in effect
        |]
      ]
    , Message "Market Participant Position" 'L'
      [ field "Stock Locate" 1 2 Integer
        [note|Locate code identifying the security|]

      , field "Tracking Number" 3 2 Integer
        [note|NASDAQ OMX internal tracking number|]

      , field "Timestamp" 5 6 Integer
        [note|Nanoseconds since midnight|]

      , field "MPID" 11 4 Alpha
        [note|Denotes the market participant identifier for which the position message is being generated|]

      , field "Stock" 15 8 Alpha [note|Stock symbol, right padded with spaces|]
      , field "Primary Market Maker" 23 1 Alpha
        [note|Indicates if the market participant firm qualifies as a Primary Market Maker in accordance with NASDAQ marketplace rules
          “Y” = primary market maker
          “N” = non-primary market maker
        |]
      , field "Market Maker Mode" 24 1 Alpha
        [note|Indicates the quoting participant’s registration
      status in relation to SEC Rules 101 and 104 of Regulation M
          “N” = normal
          “P” = passive
          “S” = syndicate
          “R” = pre-syndicate
          “L” = penalty
        |]
      , field "Market Participant State" 25 1 Alpha
        [note|Indicates the market participant’s current registration status in the issue
          “A” = Active
          “E” = Excused/Withdrawn
          “W” = Withdrawn
          “S” = Suspended
          “D” = Deleted
        |]
      ]
    , Message "MWCB Decline Level" 'V'
      [ field "Stock Locate" 1 2 Integer [note|Always set to 0|]
      , field "Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Time at which the MWCB Decline Level message was generated|]
      , field "Level 1" 11 8 Price8 [note|Denotes the MWCB Level 1 Value.|]

      , field "Level 2" 19 8 Price8 [note|Denotes the MWCB Level 2 Value.|]
      , field "Level 3" 27 8 Price8 [note|Denotes the MWCB Level 3 Value.|]
      ]
    , Message "MWCB Breach" 'W'
      [ field "Stock Locate" 1 2 Integer [note|Always set to 0|]
      , field "Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Time at which the MWCB Breaker Status message was generated|]
      , field "Breached Level" 11 1 Alpha
        [note|Denotes the MWCB Level that was breached.
          “1” = Level 1
          “2” = Level 2
          “3” = Level 3
        |]
      ]
    , Message "IPO Quoting Period Update" 'K'
      [ field "Stock Locate" 1 2 Integer [note|Always set to 0|]
      , field "Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]

      , field "Timestamp" 5 6 Integer
        [note|Time at which the IPQ Quoting Period Update message was generated|]

      , field "Stock" 11 8 Alpha
        [note|Denotes the security symbol for the issue in the NASDAQ execution system.|]
      , field "IPO Quotation Release Time" 19 4 Integer
        [note|Denotes the IPO release time, in seconds since midnight, for quotation to the nearest second.
          NOTE: If the quotation period is being canceled/postponed, we should state that
          1. IPO Quotation Time will be set to 0
          2. IPO Price will be set to 0
        |]
      , field "IPO Quotation Release Qualifier" 23 1 Alpha
        [note|Code Description
        A Anticipated quotation release time
          This value would be used when NASDAQ Market Operations initially enters the IPO instrument for release.
        C IPO release canceled/postponed
          This value would be used when NASDAQ Market Operations cancels or postpones the release of the IPO instrument.
        |]
      , field "IPO Price" 24 4 Price4
        [note|Denotes the IPO price to be used for intraday net change calculations.
          Prices are given in decimal format with 6 whole number places followed by 4 decimal digits. The whole number portion is padded on the left with spaces; the decimal portion is padded on the right with zeros. The decimal point is implied by position; it does not appear inside the price field.
        |]
      ]
    , Message "Add Order" 'A'
      [ field "Stock Locate" 1 2 Integer [note|Locate code identifying the security|]
      , field "Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Nanoseconds since midnight.|]
      , field "Order Reference Number" 11 8 Integer [note|The unique reference number assigned to the new order at the time of receipt.|]
      , field "Buy/Sell Indicator" 19 1 Alpha
        [note|The type of order being added.
          “B” = buy order.
          “S” = sell order.
        |]
      , field "Shares" 20 4 Integer
        [note|The total number of shares associated with the order being added to the book.|]
      , field "Stock" 24 8 Alpha [note|Stock symbol, right padded with spaces|]
      , field "Price" 32 4 Price4 [note|The display price of the new order.  Refer to Data Types for field processing notes.|]
      ]
    , Message "Add Order MPID Attribution" 'F'
      [ field "Stock Locate" 1 2 Integer [note|Locate code identifying the security|]
      , field "Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|
        Nanoseconds since midnight.
        |]
      , field "Order Reference Number" 11 8 Integer [note|
        The unique reference number assigned to the new order at the time of receipt.
        |]

      , field "Buy/Sell Indicator" 19 1 Alpha [note|
        The type of order being added.
          “B” = buy order.
          “S” = sell order.
        |]
      , field "Shares" 20 4 Integer [note|The total number of shares associated with the order being added to the book.|]
      , field "Stock" 24 8 Alpha [note|Stock symbol, right padded with spaces|]
      , field "Price" 32 4 Price4 [note|
        The display price of the new order.
        Refer to Data Types for field processing notes.
      |]
      , field "Attribution" 36 4 Alpha [note|NASDAQ market participant identifier associated with the entered order.|]
      ]
    , Message "Order Executed" 'E'
      [ field "Stock Locate" 1 2 Integer [note|Locate code identifying the security|]
      , field "Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Nanoseconds since midnight.|]
      , field "Order Reference Number" 11 8 Integer [note|The order reference number associated with the executed order.|]
      , field "Executed Shares" 19 4 Integer [note|The number of shares executed.|]
      , field "Match Number" 23 8 Integer [note|The NASDAQ generated day-unique Match Number of this execution. The match number is also referenced in the Trade Break Message.|]
      ]
    , Message "Order Executed With Price" 'C'
      [ field "Stock Locate" 1 2 Integer [note|Locate code identifying the security|]
      , field "Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Nanoseconds since midnight.|]
      , field "Order Reference Number" 11 8 Integer [note|The reference number of the order that was executed.|]
      , field "Executed Shares" 19 4 Integer [note|The number of shares executed.|]
      , field "Match Number" 23 8 Integer [note|The NASDAQ generated day-unique Match Number of this execution. The match number is also referenced in the Trade Break Message.|]
      , field "Printable" 31 1 Alpha [note|
        Indicates if the execution should be reflected on time and sale displays and volume calculations.
        “N” = non-printable
        “Y” = printable
      |]
      , field "Execution Price" 32 4 Price4 [note|
        The price at which the order execution occurred.
        Refer to Data Types for field processing notes.
      |]
      ]
    , Message "Order Cancel" 'X'
      [ field "Stock Locate" 1 2 Integer
        [note|Locate code identifying the security|]
      , field "Tracking Number" 3 2 Integer
        [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Nanoseconds since midnight.|]
      , field "Order Reference Number" 11 8 Integer [note|The reference number of the order being reduced.|]
      , field "Canceled Shares" 19 4 Integer [note|The number of shares being removed from the display size of the order as the result of a cancellation.|]
      ]
    , Message "Order Delete" 'D'
      [ field "Stock Locate" 1 2 Integer
        [note|Locate code identifying the security|]
      , field "Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Nanoseconds since midnight.|]
      , field "Order Reference Number" 11 8 Integer [note|The reference number of the order being canceled.|]
      ]
    , Message "Order Replace" 'U'
      [ field "Stock Locate" 1 2 Integer [note|Locate code identifying the security|]
      , field "Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Nanoseconds since midnight.|]
      , field "Original Order Reference Number" 11 8 Integer [note|The original reference number of the order being replaced.|]
      , field "New Order Reference Number" 19 8 Integer [note|
        The new reference number for this order at time of replacement.
        Please note that the NASDAQ system will use this new order reference number for all subsequent updates.
      |]
      , field "Shares" 27 4 Integer [note|The new total displayed quantity.|]
      , field "Price" 31 4 Price4 [note|
        The new display price for the order.
        Refer to Data Types for field processing notes.
      |]
      ]
    , Message "Trade Message Non-Cross" 'P'
      [ field "Stock Locate" 1 2 Integer [note|Locate code identifying the security|]
      , field " Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Nanoseconds since midnight.|]
      , field "Order Reference Number" 11 8 Integer [note|
        The unique reference number assigned to the order on the book being executed.
        Effective December 6, 2010, NASDAQ OMX will populate the Order Reference Number field within the Trade (Non-Cross) message as zero.  For the binary versions of the TotalView-ITCH data feeds, the field will be null-filled bytes (which encodes sequence of zero).
        |]
      , field "Buy/Sell Indicator" 19 1 Alpha [note|
        The type of non-display order on the book being matched.
          “B” =buy order
          “S” =sell order
        Effective 07/14/2014, this field will always be “B” regardless of the resting side.
        |]
      , field "Shares" 20 4 Integer [note|The number of shares being matched in this execution.|]
      , field "Stock" 24 8 Alpha [note|Stock symbol, right padded with spaces|]
      , field "Price" 32 4 Price4 [note|The match price of the order. Refer to Data Types for field processing notes.|]
      , field "Match Number" 36 8 Integer
        [note|The NASDAQ generated session-unique Match Number for this trade. The Match Number is referenced in the Trade Break Message.|]
      ]
    , Message "Cross Trade" 'Q'
      [ field "Stock Locate" 1 2 Integer [note|Locate code identifying the security|]
      , field "Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Nanoseconds since midnight.|]
      , field "Shares" 11 8 Integer
        [note|The number of shares matched in the NASDAQ Cross.|]
      , field "Stock" 19 8 Alpha [note|Stock symbol, right padded with spaces|]

      , field "Cross Price" 27 4 Price4
        [note|The price at which the cross occurred. Refer to Data Types for field processing notes.|]

      , field "Match Number" 31 8 Integer
        [note|The NASDAQ generated day-unique Match Number of this execution.|]

      , field "Cross Type" 39 1 Alpha [note|
        The NASDAQ cross session for which the message is being generated.
          “O” = NASDAQ Opening Cross.
          “C” = NASDAQ Closing Cross.
          “H” = Cross for IPO and halted / paused securities.
          “I” = NASDAQ Cross Network: Intraday Cross and Post-Close Cross 
        |]
      ]
    , Message "Broken Trade" 'B'
      [ field "Stock Locate" 1 2 Integer [note|Locate code identifying the security|]
      , field "Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Nanoseconds since midnight.|]
      , field "Match Number" 11 8 Integer [note|The NASDAQ Match Number of the execution that was broken. This refers to a Match Number from a previously transmitted Order Executed Message, Order Executed With Price Message, or Trade Message.|]
      ]
    , Message "Net Order Imbalance Indicator" 'I'
      [ field "Stock Locate" 1 2 Integer [note|Locate code identifying the security|]
      , field "Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Nanoseconds since midnight.|]

      , field "Paired Shares" 11 8 Integer
        [note|The total number of shares that are eligible to be matched at the Current Reference Price.|]

      , field "Imbalance Shares" 19 8 Integer
        [note|The number of shares not paired at the Current Reference Price.|]

      , field "Imbalance Direction" 27 1 Alpha [note|
        The market side of the order imbalance.
          “B” = buy imbalance
          “S” = sell imbalance
          “N” = no imbalance
          “O” = Insufficient orders to calculate
        |]

      , field "Stock" 28 8 Alpha [note|Stock symbol, right padded with spaces|]

      , field "Far Price" 36 4 Price4
        [note|A hypothetical auction-clearing price for cross orders only. Refer to Data Types for field processing notes.|]

      , field "Near Price" 40 4 Price4
        [note|A hypothetical auction-clearing price for cross orders as well as continuous orders. Refer to Data Types for field processing notes.|]

      , field "Current Reference Price" 44 4 Price4
        [note|The price at which the NOII shares are being calculated. Refer to Data Types for field processing notes.|]

      , field "Cross Type" 48 1 Alpha [note|
        The type of NASDAQ cross for which the NOII message is being generated
          “O” = NASDAQ Opening Cross
          “C” = NASDAQ Closing Cross
          “H” = Cross for IPO and halted / paused securities
        |]

      , field "Price Variation Indicator" 49 1 Alpha [note|
        This field indicates the absolute value of the percentage of deviation of the Near Indicative Clearing Price to the nearest Current Reference Price.
        “L” = Less than 1%
        “1” = 1 to 1.99%
        “2” = 2 to 2.99%
        “3” = 3 to 3.99%
        “4” = 4 to 4.99%
        “5” = 5 to 5.99%
        “6” = 6 to 6.99%
        “7” = 7 to 7.99%
        “8” = 8 to 8.99%
        “9” = 9 to 9.99%
        “A” = 10 to 19.99%
        “B” = 20 to 29.99%
        “C” = 30% or greater
        Space = Cannot be calculated
      |]
      ]
    , Message "Retail Interest" 'N'
      [ field "Stock Locate" 1 2 Integer [note|Locate code identifying the security|]
      , field "Tracking Number" 3 2 Integer [note|NASDAQ OMX internal tracking number|]
      , field "Timestamp" 5 6 Integer [note|Nanoseconds since midnight.|]
      , field "Stock" 11 8 Alpha [note|Stock symbol, right padded with spaces|]
      , field "Interest Flag" 19 1 Alpha [note|
        “B” = RPI orders available on the buy side
        “S” = RPI orders available on the sell side
        “A” = RPI orders available on both sides (buy and sell)
        “N” = No RPI orders available
        |]
      ]
    ]
}

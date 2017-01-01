{-# LANGUAGE TemplateHaskell #-}
module Protocol where

import Control.Exception.Base as GHC (assert)
import Control.Lens
import Data.Either
import Data.List (find, nub, intercalate)
import Data.Char (isPunctuation, toLower)
import Data.Maybe (fromJust)
import NeatInterpolation
import Data.String

import Utils

note = text

newtype Iden = CIden { rawIden :: String }
  deriving (Eq, Ord)

instance Show Iden where show = show . rawIden

instance Monoid Iden where
  mempty = CIden ""
  mappend (CIden x) (CIden y) = CIden $ x ++ "_" ++ y
instance IsString Iden where fromString = iden

iden, cname :: (Show s) => s -> Iden
iden s = CIden $ intercalate "_" . words $ foreach (show s) $ \c ->
  if isPunctuation c
    then ' '
    else c

cname = iden . map toLower . show

data Field a = Field
  { _ofst    :: Int     -- wire offset of field
  , _len     :: Int     -- wire len of field
  , _name    :: String  -- field name
  , _atype   :: a       -- abstract 'type' to be mapped to target arch
  , _notes   :: String  -- any notes. suggest you use [note||] quasiquote
  } deriving (Eq, Show, Ord)
data Message a = Message
  { _msgName :: String
  , _tag     :: Char -- todo make this a type variable
  , _fields  :: [Field a]
  } deriving (Eq, Show, Ord)

-- data Proxy a = Proxy deriving Show
data Proto a = Proto
  { _namespace        :: String
  , _lineSeparated    :: Bool
  , _pktHdrLen        :: Int
  , _incomingMessages :: [Message a]
  , _outgoingMessages :: [Message a]
  } deriving (Eq, Ord)

instance Show (Proto a) where
  show = _namespace

makeLenses ''Proto
makeLenses ''Field
makeLenses ''Message

allMessages :: Proto a -> [Message a]
allMessages p = p^.incomingMessages ++ p^.outgoingMessages

msgMaybe         :: Eq a => (String -> a) -> Proto t -> String -> Maybe (Message t)
msgMaybe eqclass proto name  = GHC.assert noDupMsgNames findMsg where
  noDupMsgNames = let
    names = (map (view msgName) $ allMessages proto)
    in nub names == names
  findMsg = flip find (allMessages proto) $
    \msg -> eqclass name == eqclass (msg^.msgName)

msg              :: Eq a => (String -> a) -> Proto t -> String -> Message t
msg eqclass p name  = maybe (error errMsg) id $ msgMaybe eqclass p name where
                       errMsg = "no msg "++name++" in "++(p^.namespace.to show)

getField :: Eq a => (String -> a) -> String -> Proto t -> String -> Field t
getField eqclass name proto msgname =
  maybe (error errMsg) id findField where

    table = view fields $ msg eqclass proto msgname
    findField = find (\f -> eqclass (f&_name) == eqclass name) $ table

    errMsg = "couldn't find "++name++" in "++(proto^.namespace^.to show)++"."++msgname

messageValid :: (Show a) => Message a -> Either String (Message a)
messageValid msg = let
  lengths = scanl (+) 0 $ msg ^. fields.to (map _len)
  ofsts   = id          $ msg ^. fields.to (map _ofst)
  in if (all id $ zipWith (==) lengths ofsts)
    then Right msg
    else Left $ unlines
      ["Offsets don't add up!\n", show lengths, show ofsts, msg ^. msgName]

checkMessagesValid :: (Show a) => Proto a -> IO ()
checkMessagesValid a = let
  eithers = map messageValid (allMessages a)
  in if all isRight eithers
    then return ()
    else error $ concat $ lefts eithers

-- takes a mesage and sets the offsets of the fields
-- so that they match with the lengths of the fields.
-- e.g.
-- field1 length 2
-- field2 length 8
-- field3 length 5
-- will map to
-- offset 0 (start at 0)
-- offset 2 (0+2)
-- offset 10 (0+2+8)
reifyOffsets :: Message a -> Message a
reifyOffsets msg = msg & fields %~ \fs -> let
  new_ofsts = fs
    & map _len
    & scanl (+) 0
  in zip fs new_ofsts
    & map (\(f, new_ofst) -> f & set ofst new_ofst)

bodyLen :: Proto a -> Message a -> Int
bodyLen p msg = msg^.fields
  & map _len
  & sum
  & (if _lineSeparated p then (+1) else id)

tagLen :: Proto a -> Int
tagLen _ = 1
pktLen :: Proto a -> Message a -> Int
pktLen p m = tagLen p + _pktHdrLen p + bodyLen p m

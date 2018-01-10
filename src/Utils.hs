module Utils where
import Data.List
import Data.Maybe
import Data.Function
import Data.Char
import System.IO
import System.IO.Unsafe
import Data.Time.Clock
import System.Exit (ExitCode(..))
import Control.Monad.IO.Class
import Control.Concurrent.STM (atomically)
import Control.Exception
import Data.ByteString.Lazy.Char8 as B8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

foreach :: [a] -> (a -> b) -> [b]
foreach = flip map

switch :: a -> a -> Bool -> a
switch t f p = if p then t else f

snoc :: [a] -> a -> [a]
snoc xs y = xs ++ [y]

trim :: String -> String
trim = trimL . trimR
  where
    trimL = dropWhile isSpace
    trimR = reverse . trimL . reverse

-- shamelessly taken from neat-interpolation
dedent :: String -> String
dedent s = case minimumIndent s of
  Just len -> unlines $ map (drop len) $ lines s
  Nothing  -> s
  where
    minimumIndent = listToMaybe . sort . map (length . takeWhile (==' '))
      . filter (not . null . dropWhile isSpace) . lines

assert :: Bool -> a -> a
assert pred a = if pred then a else (error "Assertion failed")

maybeHead :: Foldable t => t a -> Maybe a
maybeHead = foldr (\a _ -> Just a) Nothing

timer :: String -> IO a -> IO a
timer msg action = do
  t0 <- getCurrentTime
  ret <- action
  t1 <- getCurrentTime
  hPutStrLn stderr $ msg ++ " took " ++ show (t1 `diffUTCTime` t0)
  return ret

todo :: String -> a
todo = error . ("TODO: " ++)
           
traceWith :: (a -> String) -> a -> a
traceWith f a = unsafePerformIO $ putStrLn (f a) >> return a
           
trace :: Show a => String -> a -> a
trace s = traceWith $ ((s++": ")++) . show

decodeUtf8String :: ByteString -> String
decodeUtf8String = T.unpack . T.decodeUtf8 . B8.toStrict

encodeUtf8String :: String -> ByteString
encodeUtf8String = B8.fromStrict . T.encodeUtf8 . T.pack

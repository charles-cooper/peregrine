module Utils where
import Data.List
import Data.Maybe
import Data.Char
import System.IO
import Data.Time.Clock

foreach :: [a] -> (a -> b) -> [b]
foreach = flip map

switch :: a -> a -> Bool -> a
switch t f p = if p then t else f

snoc :: [a] -> a -> [a]
snoc xs y = xs ++ [y]

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


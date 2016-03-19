module Utils where
import Data.List
import Data.Maybe
import Data.Char

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



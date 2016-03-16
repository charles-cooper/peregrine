module Utils where

foreach :: [a] -> (a -> b) -> [b]
foreach = flip map

switch :: a -> a -> Bool -> a
switch t f p = if p then t else f

snoc :: [a] -> a -> [a]
snoc xs y = xs ++ [y]

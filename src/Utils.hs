module Utils where

foreach :: [a] -> (a -> b) -> [b]
foreach = flip map


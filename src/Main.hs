{-# LANGUAGE QuasiQuotes #-}
module Main where
import Language.C.Quote.C
import Language.C.Smart
import Text.PrettyPrint.Mainland (putDocLn, ppr)

foo = [cexp|x + 1|]
bar = [cexp|y|]
baz = bar + foo

main = do
  putDocLn $ ppr baz

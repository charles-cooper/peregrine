{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Text.InterpolatedString.Perl6
import Language.C.Quote.C
import Language.C.Smart
import Text.PrettyPrint.Mainland (putDocLn, ppr)

foo = [cenum|y = 1|]
bar = [cdecl|int x;|]
foobar = [cexp|foobar[123]|]
baz width = [cty|typename $id:(ty::String)|]
  where
    ty :: String
      | width == 1 = "int8_t"
      | width == 2 = "int16_t"
      | width <= 4 = "int32_t"
      | width <= 8 = "int64_t"

-- include :: String -> Definition
include str = [cedecl|$esc:(include')|]
  where
    include' = [qc|#include {str}|]

qux = [cunit|
$edecl:(include "stdio.h")

// $decl:bar;

// Hello

// int x = $init:foobar;

enum foo {
  x = 2,
  $enum:foo,
};

int fun(int x) {
  bar[12] = x;
  return 1 /* comment */;
}
|]


main = do
  putDocLn $ ppr qux
  print foobar


{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.C.Lib where
import Text.InterpolatedString.Perl6 (qc)
import Language.C.Quote.C
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland
import Language.C.Utils

cReadIntegral ty = do
  mapM include ["cstdint", "cassert", "cctype"]
  depends impl
  where
    funName :: String = [qc|parse_{pretty 0 $ ppr ty}|]
    impl = [cfun|
      $ty:ty $id:(funName) (char const *buf, $ty:uint len) {
        $ty:ty ret = 0;
        while (len--) {
          assert(isdigit(*buf));
          ret = ret * 10 + (*buf - '0');
          ++buf;
        }
        return ret;
      }
    |]

-- C++ stuff
showc :: Pretty c => c -> String
showc = pretty 0 . ppr

cpp_string :: C C.Type
cpp_string = do
  include "string"
  return [cty|typename $id:("std::string")|]

cpp_unordered_map :: C.Type -> C.Type -> C C.Type
cpp_unordered_map k v = do
  include "unordered_map"
  return [cty|typename $id:ty|]
  where
    ty :: String = [qc|std::unordered_map<{showc k}, {showc v}>|]



{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.C.Lib where
import Data.String.Interpolate
import Language.C.Utils

cReadIntegral ty = do
  mapM include ["cstdint", "cassert", "cctype"]
  cfun impl
  return funName
  where
    funName = [i|parse_${ty}|]
    impl = [i|
      ${ty} ${funName} (char const *buf, ${uint} len) {
        ${ty} ret = 0;
        while (len--) {
          assert(isdigit(*buf));
          ret = ret * 10 + (*buf - '0');
          ++buf;
        }
        return ret;
      }
    |]

cpp_string :: C Type
cpp_string = do
  include "string"
  return $ Type [i|std::string|]

cpp_unordered_map :: Type -> Type -> C Type
cpp_unordered_map k v = do
  include "unordered_map"
  return $ Type [i|std::unordered_map<${k}, ${v}>|]


{-# LANGUAGE QuasiQuotes #-}
module Protocol.Nasdaq.ITCH.Proto.C where

import Protocol
import Protocol.Nasdaq.ITCH.Proto

import           Protocol.Backend.C.Base as C

import           Language.C.Utils as C
import           Data.String.Interpolate

itchCSpec :: C.Specification ITCH
itchCSpec = C.Specification
  { _proto      = itch
  , _mkTy       = mkTy
  , _readMember = readMember
  }

mkTy :: Field ITCH -> C C.GType
mkTy f@(Field _ len _ ty _) = do
  include "cstdint"
  let ret
        | ty==Alpha   && len==1 = C.SimpleTy char
        | ty==Alpha             = C.ArrayTy char len
        | ty==Integer && len==2 = C.SimpleTy ushort
        | ty==Integer && len==4 = C.SimpleTy uint
        | ty==Integer && len==6 = C.SimpleTy ulong
        | ty==Integer && len==8 = C.SimpleTy ulong
        | ty==Price4  && len==4 = C.SimpleTy uint
        | ty==Price8  && len==8 = C.SimpleTy ulong
        | otherwise             = error $ "No rule to make type for "++(show f)
  return ret

readMember :: C.Exp -> C.Exp -> Field ITCH -> C Code
readMember dst src f@(Field _ len _ ty _) = case ty of
  Alpha   -> if len == 1
   then pure [i|${dst} = *${src};|]
   else do
     include "cstring"
     return [i|memcpy(&${dst}, ${src}, ${len});|]
  Integer -> runIntegral len
  Price4  -> runIntegral len
  Price8  -> runIntegral len
  where
   runIntegral len = do
     include "cstring"
     include "endian.h"
     return $ case len of
       2 -> [i|${dst} = be16toh(*(uint16_t*)${src});|]
       4 -> [i|${dst} = be32toh(*(uint32_t*)${src});|]
       6 -> [i|{
           ${dst} = 0;
           memcpy(&${dst}, ${src}, ${len});
           $dst = be64toh(${dst}) >> 8*(sizeof(${dst}) - ${len});
         }|]
       8 -> [i|${dst} = be64toh(*(uint64_t*)${src});|]
       _ -> error "Unknown integral length."


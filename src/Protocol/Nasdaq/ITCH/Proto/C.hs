{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
module Protocol.Nasdaq.ITCH.Proto.C where

import Protocol
import Protocol.Nasdaq.ITCH.Proto

import           Protocol.Backend.C.Base as C

import           Language.C.Utils as C
import           Language.C.Lib as CL
import           Data.String.Interpolate

itchCSpec :: C.Specification ITCH
itchCSpec = C.Specification
  { _proto      = itch
  , _mkTy       = mkTy
  , _readMember = readMember
  }

mkTy :: Field ITCH -> C C.Type
mkTy f@(Field _ len _ ty _) = do
  include "cstdint"
  if
    | ty==Alpha   && len==1 -> pure char
    | ty==Alpha             -> CL.arrayTy char len
    | ty==Integer && len==2 -> return ushort
    | ty==Integer && len==4 -> return uint
    | ty==Integer && len==6 -> return ulong
    | ty==Integer && len==8 -> return ulong
    | ty==Price4  && len==4 -> return uint
    | ty==Price8  && len==8 -> return ulong
    | otherwise             -> error $ "No rule to make type for "++(show f)

readMember :: Field ITCH -> C.Exp -> C.Exp -> C Code
readMember f@(Field _ len _ ty _) dst src = case ty of
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


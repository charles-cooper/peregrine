{-# LANGUAGE QuasiQuotes #-}
module Main where
import Protocol.Tmx.TAQ as TAQ
import Protocol
import Language.C.Quote.C
import Language.C.Smart ()
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland (putDocLn, ppr)

import Control.Lens
import Control.Monad (forM)

char   = [cty|char|]
uchar  = [cty|typename uint8_t|]
bool   = [cty|typename bool|]
ushort = [cty|typename uint16_t|]
uint   = [cty|typename uint32_t|]
ulong  = [cty|typename uint64_t|]

assert :: Bool -> a -> a
assert pred a = if pred then a else (error "Assertion failed")

genStruct :: Message TAQ -> C.Type
genStruct msg = [cty|
  struct $id:(msg^.msgName) {
    $sdecls:decls
  }
  |]
  where
    decls = mkDecl <$> (msg^.fields)
    mkDecl f@(Field _ len nm ty notes)
      | ty==Numeric    && len==9 = [csdecl|$ty:uint   $id:cnm;|]
      | ty==Numeric    && len==7 = [csdecl|$ty:uint   $id:cnm;|]
      | ty==Numeric    && len==3 = [csdecl|$ty:ushort $id:cnm;|]
      | ty==Numeric              = error $ "Unknown integer len for " ++ show f
      | ty==Alphabetic && len==1 = [csdecl|$ty:char   $id:cnm;|]
      | ty==Alphabetic           = [csdecl|$ty:char   $id:cnm[$const:len];|]
      | ty==Boolean              = assert (len==1) [csdecl|$ty:bool $id:cnm;|]
      | ty==Date                 = [csdecl|$ty:uint   $id:cnm;|]
      | ty==Time                 = [csdecl|$ty:uint   $id:cnm;|]
      where
        cnm = rawIden (cname nm)

main = do
  forM (taq^.outgoingMessages) $ putDocLn . ppr . genStruct

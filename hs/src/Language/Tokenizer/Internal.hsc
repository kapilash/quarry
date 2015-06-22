{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Language.Tokenizer.Internal where

import Foreign
import Foreign.C.Types
import qualified Data.ByteString.Char8 as CB
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as E
import qualified Data.Text.Foreign as TF
import Foreign.Storable
import Foreign.C.String

#include <Quarry.h>

newtype QLang = QLang {unLang :: CInt}
              deriving (Eq, Show)

qC =  QLang 0

qJava = QLang 1

newtype QReader = QReader (Ptr QReader)

newtype QToken = QToken (Ptr QToken)

fromFile :: FilePath -> QLang -> IO QReader
fromFile file lang = undefined


fromText :: Txt.Text -> QLang -> IO QReader
fromText txt lang = undefined


foreign import ccall "Quarry.h quarry_Close"
        _close :: Ptr QReader -> IO ()

foreign import ccall "Quarry.h quarry_fromFile"
        _fromFile :: QLang ->CString -> IO (Ptr QReader)

foreign import ccall "Quarry.h quarry_fromString"
        _fromString :: QLang -> CString -> CInt -> CInt -> CInt -> IO (Ptr QReader)                     

foreign import ccall "Quarry.h quarry_nextToken"
        _nextToken :: Ptr QReader -> IO (Ptr QToken)

foreign import ccall "Quarry.h quarry_freeToken"
        _freeToken :: Ptr QToken -> IO ()

                      

data NativeToken = SimpleToken !Int !Int !Int 
                   | TokenWithText !Int !Int !Int !Txt.Text
                  deriving (Eq, Show)

peekNextToken :: QReader -> IO NativeToken
peekNextToken (QReader reader) = do
  ptr <- _nextToken reader
  line <- #{peek struct quarry_Token, line}
  col  <- #{peek struct quarry_Token, column}
  tokenType <- #{peek struct quarry_Token, tokenType}
  tokLen    <- #{peek struct quarry_Token, length}
  tptr       <- #{peek struct quarry_Token, textPtr}
  opq       <- #{peek struct quarry_Token, opaque}
  txt      <- TF.peekCStringLen (tptr,tokLen)
  _freeToken ptr
  return $ TokenWithText line col tokenType txt


{-

instance Storable NativeToken where
  sizeof _ = #{size quarry_Token}
  alignment _ = #{alignment quarry_Token}
  peek ptr  = do
    line <- #{peek quarry_Token, line}
    col  <- #{peek quarry_Token, column}
    tokenType <- #{peek quarry_Token, tokenType}
    tokLen    <- #{peek quarry_Token, length}
    tptr       <- #{peek quarry_Token, textPtr}
    opq       <- #{peek quarry_Token, opaque}
    txt      <- TF.peekCStringLen (tptr,tokLen)
    return $ NativeToken line col tokenType txt
  poke ptr (NativeToken line col tokenType txt
    
  
-}

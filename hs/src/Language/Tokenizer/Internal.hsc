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
fromFile file lang = withCAString file $ \x -> do
   r <- _fromFile lang x
   return $ QReader r
  


fromText :: Txt.Text -> QLang -> IO QReader
fromText txt lang = undefined

closeReader :: QReader -> IO  ()
closeReader (QReader r) = _close r 

foreign import ccall "Quarry.h quarry_close"
        _close :: Ptr QReader -> IO ()

foreign import ccall "Quarry.h quarry_fromFile"
        _fromFile :: QLang ->CString -> IO (Ptr QReader)

foreign import ccall "Quarry.h quarry_fromStr"
        _fromString :: QLang -> CString -> CInt -> CInt -> CInt -> IO (Ptr QReader)                     

foreign import ccall "Quarry.h quarry_nextToken"
        _nextToken :: Ptr QReader -> IO (Ptr QToken)

foreign import ccall "Quarry.h quarry_freeToken"
        _freeToken :: Ptr QToken -> IO ()

                      

data NativeToken = SimpleToken !CInt !CInt !CInt 
                   | TokenWithText !CInt !CInt !CInt !Txt.Text
                  deriving (Eq, Show)

isEOF (SimpleToken _ _ i) = i > 22
isEOF (TokenWithText _ _ i _) = i > 22

readNextToken :: QReader -> IO NativeToken
readNextToken (QReader reader) = do
  ptr <- _nextToken reader
  line  <- #{peek struct quarry_Token, line} ptr 
  col  <- #{peek struct quarry_Token, column} ptr
  tokenType <- #{peek struct quarry_Token, tokenType} ptr
  tokLen    <- #{peek struct quarry_Token, length} ptr
  tptr       <- #{peek struct quarry_Token, textPtr} ptr
  txt      <- TF.peekCStringLen (tptr,tokLen)
  _freeToken ptr
  return $ TokenWithText line col tokenType txt


printTokens :: QReader -> IO ()
printTokens q = do
  t <- readNextToken q
  if (isEOF t)
     then return ()
    else do
    print t
    printTokens q

testFile :: FilePath -> IO ()
testFile s = do
  r <- fromFile s qJava
  printTokens r
  closeReader r

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

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
import qualified Data.Iteratee as Iter
import Control.Exception
import Data.NullPoint
import Control.Monad.IO.Class
import qualified Control.Monad.Catch as CIO    

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

                      

data NativeToken = NToken !CInt !CInt !CInt !Txt.Text
                  deriving (Eq, Show)


instance Exception NativeToken

{-
instance NullPoint NativeToken where
  empty = NToken 0 0 (-1) (Txt.empty) -}

isEOF (NToken _ _ i _) = i > 22

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
  return $ NToken line col tokenType txt


readNTokens :: [NativeToken] -> Int -> QReader -> IO (Either SomeException [NativeToken])
readNTokens lst 0 _  = return $ Right (reverse lst)
readNTokens lst num (QReader reader) = do
  ptr <- _nextToken reader
  line  <- #{peek struct quarry_Token, line} ptr 
  col  <- #{peek struct quarry_Token, column} ptr
  tokenType <- #{peek struct quarry_Token, tokenType} ptr
  tokLen    <- #{peek struct quarry_Token, length} ptr
  tptr       <- #{peek struct quarry_Token, textPtr} ptr
  txt      <- TF.peekCStringLen (tptr,tokLen)
  _freeToken ptr
  if tokenType < 1
     then return $ Left . toException $ NToken line col tokenType txt
    else if (tokenType > 22)
         then return $ Right (reverse lst)
         else readNTokens ((NToken line col tokenType txt):lst) (num - 1) (QReader reader)



readCallback :: (MonadIO m) => Int -> QReader -> m (Either SomeException ((Bool, QReader), [NativeToken]))
readCallback n  qr@(QReader reader) = do
  etokens <- liftIO $ readNTokens [] n qr
  case etokens  of
   Right []   -> return $ Right ((True, qr), [])
   Right (tokens) -> return $ Right (((length tokens) == n, qr), tokens)
   Left x              -> return (Left x)



enumFile :: Int -> FilePath -> QLang -> Iter.Enumerator [NativeToken] IO a
enumFile i filePath lang iter = bracket
                                (fromFile filePath lang)
                                (closeReader)
                                (\r -> Iter.enumFromCallback (readCallback i)  r iter)

{-fileDriver :: Int ->  FilePath -> QLang -> Iter.Iteratee [NativeToken] IO a -> IO a
fileDriver num filePath lang iter = do
   i <- bracket  (fromFile filePath lang)  closeReader (\r -> Iter.enumFromCallback (readCallback num) r iter)
   Iter.run i -}

fileDriver :: (MonadIO m, CIO.MonadMask m) => Int ->  FilePath -> QLang -> Iter.Iteratee [NativeToken] m a -> m a
fileDriver num filePath lang iter = do
   i <- CIO.bracket  (liftIO $ fromFile filePath lang)  (liftIO . closeReader) (\r -> Iter.enumFromCallback (readCallback num) r iter)
   Iter.run i       
                                
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

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable NativeToken where
  sizeOf _ = #{size quarry_TokenStr}
  alignment _ = #{alignment quarry_TokenStr}
  peek ptr  = do
      line  <- #{peek struct quarry_Token, line} ptr 
      col  <- #{peek struct quarry_Token, column} ptr
      tokenType <- #{peek struct quarry_Token, tokenType} ptr
      tokLen    <- #{peek struct quarry_Token, length} ptr
      tptr       <- #{peek struct quarry_Token, textPtr} ptr
      txt      <- TF.peekCStringLen (tptr,tokLen)
      return $ NToken line col tokenType txt

  poke ptr (NToken line col tokenType txt) = TF.withCStringLen txt $ \(tptr,tokLen) -> do
    #{poke struct quarry_Token, line} ptr line
    #{poke struct quarry_Token, column} ptr col
    #{poke struct quarry_Token, tokenType} ptr tokenType
    #{poke struct quarry_Token, length} ptr tokLen
    #{poke struct quarry_Token, textPtr} ptr tptr


  
    

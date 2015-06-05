module Text.QLexer where


import qualified Data.Text as Txt
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS    
import Control.Monad.State
import Text.QToken
import Data.Bits
import Data.Word
import Control.Applicative    

newtype Position = Position (Int, Int)
    deriving (Eq,Show)
    
nextLine (Position (x,_)) = Position (x+1,1)

addCol w (Position (x, y)) = Position (x, y + w)

incrColumnBy :: Int -> Parser ()                             
incrColumnBy = modify . addCol

incrLine ::  Parser ()                             
incrLine = modify nextLine
       
type Parser  = StateT Position P.Parser

space :: Parser ()     
space = do
      lift $ P.skip $ \w -> (w /= 10) && (w < 33)
      incrColumnBy 1


             
newLine :: Parser ()
newLine = do
        lift $ P.word8 10
        incrLine

asRecord :: QToken -> Parser QRecord
asRecord t = do
         Position (l,c) <- get
         return $ QRecord t l c

parseUTF :: Parser Char
parseUTF = do
  (c, i) <- lift $ parseUTF'
  incrColumnBy i
  return $ toEnum c

         

toHex = BS.foldl f 0 .  BS.map w8ToDigit
     where
       f i w = i * 16 +  (fromEnum w)
       w8ToDigit w = if (w > 47) && (w < 58)
                     then w - 47
                     else if (w  > 64) && ( w < 71)
                          then w - 55
                          else w - 87


                               
parseEsc :: P.Parser (Int, Int)
parseEsc = do
      P.word8 92
      escaped <|> utf
  where
    escaped = do
             w <- P.satisfy (flip elem [34,39,48, 92,97,98,102,114,110,116,118])
             return (fromEnum w, 2)

    utf = do
       P.satisfy (\w -> (w == 117 || w == 85 || w == 120))
       (i,c) <- parseHexes
       return (i, c+2)

             
parseChar :: P.Parser (QToken, Int, Int)
parseChar = do
  P.word8 39
  (i,c) <- parseEsc <|> parseUTF'
  P.word8 39
  return $ (QCharLiteral $ toEnum i, 0, c + 2)

parseCStr :: P.Parser (QToken, Int, Int)
parseCStr = do
  P.word8 34
  lst <- P.manyTill (parseEsc <|> parseUTF') (P.word8 34)
  return $ (QStrLiteral $ map (toEnum . fst) lst, 0, length lst)
  
parseHexes :: P.Parser (Int, Int)
parseHexes = do
  b <- P.takeWhile1 $ \w ->
                          (or [( w > 47) && (w <58),
                               (w > 64) && (w <71),
                               (w > 96) && (w < 103) ])
  return $ (toHex b,  BS.length b)
                  
parseUTF' :: P.Parser (Int, Int)                
parseUTF' = do { i <- parseUtf1; return (i,1) }
           <|> do { i<- parseUtf2; return (i,2) }
           <|> do { i <- parseUtf3; return (i,3) }
           <|> do { i <- parseUtf4; return (i,4) }
         where  
           parseUtf1 = do
             b <-  P.satisfy (< 128)
             return $ fromEnum b

           parseUtf2 = do
                v <- asInt (\w -> (w > 191) && (w < 223))
                t <- asInt   (< 192)
                return $ (shift v 6) + t - 0x3080
 
           parseUtf3 = do
                v <- asInt (\w -> (w > 223) && (w < 239))
                t1 <- asInt   (< 192)
                t2 <- asInt  (< 192)
                return $ (shift v 12) + (shift t1 6) + t2  - 0xE2080
        
           parseUtf4 = do
                v <- asInt (\w -> (w > 191) && (w < 223))
                t1 <- asInt   (< 192)
                t2 <- asInt  (< 192)
                t3 <- asInt (< 192)
                return $ (shift v 18) + (shift t1 12) + (shift t2 6) + t3  - 0x3C82080
                
           asInt :: (Word8 -> Bool) -> P.Parser Int
           asInt = fmap fromEnum . P.satisfy

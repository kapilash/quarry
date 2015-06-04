module Text.QLexer where


import qualified Data.Text as Txt
import qualified Data.Attoparsec.ByteString as P
import Control.Monad.State
import Text.QToken       

newtype Position = Position (Int, Int)
    deriving (Eq,Show)
    
nextLine (Position (x,_)) = Position (x+1,1)

addCol w (Position (x, y)) = Position (x, y + w)

       
type Parser  = StateT Position P.Parser

space :: Parser ()     
space = do
      lift $ P.skip $ \w -> (w /= 10) && (w < 33)
      modify (addCol 1)

newLine :: Parser ()
newLine = do
        lift $ P.word8 10
        modify nextLine

asRecord :: QToken -> Parser QRecord
asRecord t = do
         Position (l,c) <- get
         return $ QRecord t l c

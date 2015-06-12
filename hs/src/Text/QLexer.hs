module Text.QLexer where

import Text.QToken
import Control.Applicative
import Data.Functor
import Control.Monad
import qualified Data.List as Lst

data LexInput = LexInput !String !Int !Int
                deriving Show

data Lexer a = Lexer { unLex :: LexInput -> ((Either String a), LexInput) }


retLexer :: a -> Lexer a
retLexer x = Lexer $ \input -> (Right x, input)

bindLexer :: Lexer a -> (a -> Lexer b) -> Lexer b
bindLexer (Lexer aLexer) fxToLY = Lexer $ \input ->
  case aLexer input of
   (Left msg, i)  -> (Left msg, i)
   (Right a, leftOver) -> case unLex (fxToLY a) $ leftOver of
                            (Left msg, _)     -> (Left msg, input)
                            r                 -> r

failMsg s = Lexer $ \input -> (Left s, input)

mapLexer :: (a -> b) -> Lexer a -> Lexer b
mapLexer fAToB (Lexer aLex) = Lexer $ \input ->
  case aLex input of
  (Left msg, i)     -> (Left msg, i)
  (Right aVal, leftOver) -> (Right $ fAToB aVal, leftOver)

instance Functor Lexer where
  fmap = mapLexer

seqLexers :: Lexer (a -> b) -> Lexer a -> Lexer b
seqLexers (Lexer a2bF) (Lexer aF) = Lexer $ \input ->
  case a2bF input of
   (Left msg, i)            -> (Left msg, i)
   (Right a2bVal, leftOver) -> case aF leftOver of
                                (Right aVal, remaining) -> (Right $ a2bVal aVal, remaining)
                                (Left msg, _)           -> (Left msg, input)


choose :: Lexer a -> Lexer a -> Lexer a
choose (Lexer a1F) (Lexer a2F) = Lexer $ \input ->
  case a1F input of
   (Right av, leftOver)  -> (Right av, leftOver)
   (Left _, _)           -> a2F input

instance Applicative   Lexer where
  pure = retLexer
  (<*>) = seqLexers

instance Monad Lexer where
  return = retLexer
  (>>=) = bindLexer
  fail  = failMsg

instance Alternative Lexer where
  empty = failMsg "no-parse"
  (<|>) = choose

instance MonadPlus Lexer where
  mzero = failMsg "no-parse"
  mplus = choose

newLine = Lexer newLine'
  where
    newLine' i@(LexInput ('\r':'\n':rest) l _) = (Right (), LexInput rest (l+1) 0)
    newLine' i@(LexInput ('\n':rest) l _)      = (Right (), LexInput rest (l+1) 0)
    newLine' i  = (Left "Expected EOL", i)

char :: Char -> Lexer Char
char c = Lexer char'
  where char' i@(LexInput (a:rest) ln col) = if a == c then  (Right a, LexInput rest ln (col + 1))
                                             else (Left $ "Expected " ++ (show c) ++ " found " ++ (show a), i)
        char' i                         = (Left $ "EOF while looking for " ++ show c, i)

peek :: Lexer Char
peek  = Lexer peek'
  where peek' i@(LexInput (a:rest) ln col) = (Right a, i)
        peek' i                            = (Left "EOF", i)
        
anyChar ::  Lexer Char
anyChar = do
   c <- peek
   if c == '\r' || (c == '\n')
     then (newLine >> return '\n')
     else char c

keyword :: String -> Lexer String
keyword str = Lexer kw'
  where kw' i@(LexInput text l c) = case Lst.stripPrefix str text of
                                     Just rst     -> (Right str, LexInput rst l (c + (length str)))
                                     Nothing      -> (Left $ "Expected " ++ str, i)

while :: (Char -> Bool) -> Lexer String
while pred = Lexer $ \input -> wh' input []
  where wh' (LexInput [] l c) sofar = (Right $ Lst.reverse sofar, LexInput [] l c)
        wh' i@(LexInput (a:rest) l c) sofar = if pred a
                                            then if a == '\n' then wh' (LexInput rest (l+1) 0) (a:sofar)
                                                 else wh' (LexInput rest l (c+1)) (a:sofar)
                                            else (Right $ Lst.reverse sofar, i)

while1 :: (Char -> Bool) -> Lexer String
while1 pred = do
  s <- while pred
  case s of
   []   -> fail "unsatisfied condition"
   _    -> return s

till :: Lexer a -> Lexer String
till p = till'  []
   where till' sofar = (pwrap sofar) <|> (readOne sofar)
         pwrap sofar = do {p; return $ (Lst.reverse sofar)}
         readOne l = do 
           ac <- anyChar
           till' (ac:l)

skipTill :: Lexer a -> Lexer ()
skipTill p = ( p >> return ()) <|> (anyChar >> (skipTill p))

csComments = csLineComment <|> csBlockComment
       where csLineComment = do {keyword "//"; skipTill newLine}
             csBlockComment = do {keyword "/*"; skipTill (keyword "*/")}


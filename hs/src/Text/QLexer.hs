module Text.QLexer where

import Text.QToken
import Control.Applicative
import Data.Functor
import Control.Monad
import qualified Data.List as Lst
import Data.Char

{-
Tried to integrate with the parsers library. But encountered infinite loops.
For instance - calling integer of Text.Parser.Token.integer

I suspect this is to do with the way bindLexer is below. We have built-in backtracking.
We do not need try.

Another problem with the parsers library is  that the tokens are all based on Haskell Report. For instance, integer is defined
based on the syntax of integer literals in Haskell. It means, no binaries. 
I really want support for multiple Languages. So we are ignoring this for now.
Only nagging issue is - we still do not know if my approach is going to hurt me later on.

import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Char
import qualified Text.Parser.Token as T
-}

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

matches :: (Char -> Bool) -> Lexer Char
matches pred =   Lexer matches'
  where matches' i@(LexInput (a:rest) ln col) = if (pred a) && (a == '\n')
                                                then (Right a, LexInput rest (ln+1) 0 )
                                                else if pred a then (Right a, LexInput rest ln (col + 1))
                                                     else (Left "predicate fail", i)
        matches' i                            = (Left "EOF", i)

        
char :: Char -> Lexer Char
char c = matches (== c)

isMatchFor :: (Char -> Bool) -> Lexer Bool
isMatchFor pred = Lexer matches'
   where matches' i@(LexInput (a:rest) ln col) = (Right (pred a), i)
         matches' i                            = (Right False, i)

optDefault :: Lexer a -> a -> Lexer a
optDefault lexer defval = lexer <|> (return defval)

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
       where csLineComment = do { keyword "//"; many (matches $ \x -> x /= '\r' && x /= '\n'); newLine }
             csBlockComment = do {keyword "/*"; skipTill (keyword "*/")}


sign :: Lexer Char
sign =  optDefault plusOrMinus '+'
   where plusOrMinus = (char '+') <|> (char '-')

numPrefix :: (Num a) => Lexer a
numPrefix = do
    c <- sign
    if c == '+' 
        then return 1
        else return (-1)

digits :: Lexer String         
digits = while1 isDigit

hexDigits = while1 isHexDigit
 where f c = (c >= '0' && c <= '9') 
             || (c >= 'A' && c <= 'F')
             || (c >= 'a' && c <= 'f')
       

double :: Lexer Double
double =   do
  inits <- digits
  char '.'
  dgts <- digits
  power <- exp <|> (return "")
  return . read $  inits ++ ('.' : dgts) ++ power
  where
    exp = do
      c <- matches (\x -> x == 'E' || x == 'e')
      s <- sign
      dgts <- digits
      return $ c:s:dgts

csDouble :: Lexer QToken
csDouble = do
    d <- double
    sfx <- optional (suffixM <|> suffixD <|> suffixF <|> suffixL)
    return $ QDouble  d sfx
 where
    suffixM = (matches (\x -> x == 'M' || x == 'm')) >> return SuffixM
    suffixF = (matches (\x -> x == 'F' || x == 'f')) >> return SuffixF
    suffixD = (matches (\x -> x == 'D' || x == 'd')) >> return SuffixD
    suffixL = (matches (\x -> x == 'L' || x == 'l')) >> return SuffixD


hexaDecimal :: Lexer Integer
hexaDecimal = do
  char '0'
  matches (\x -> x == 'x' || x == 'X')
  dgts <- hexDigits
  return $ read ("0X" ++ dgts)

octal :: Lexer Integer
octal = do
  char '0'
  matches (\x -> x == 'O' || x == 'o')
  dgts <- while1 isOctDigit
  return . read $ ("0O" ++ dgts)

binary :: Lexer Integer
binary = do
  char '0'
  matches (\x -> x == 'b' || x == 'B')
  bools <-    while1 (\x -> x == '0' || x == '1')
  return . foldl f 0 . map (\x -> x == '1') $ bools
 where
   f x True = 2*x + 1
   f x _    = 2*x
  
decimal :: Lexer Integer
decimal = read <$> digits 

integral = hexaDecimal <|> octal <|> binary <|> decimal

csIntegral :: Lexer QToken 
csIntegral = do
    i <- integral
    sfx <- optional (suffixUL <|> suffixLU <|> suffixU <|> suffixL)
    return $ QIntegral i sfx
  where
     u = matches (\x -> x == 'U' || x == 'u')
     l' = matches (\x -> x == 'L' || x == 'l')
     l  = l' >> (optional l')
     suffixUL = (u >> l >> return SuffixUL)
     suffixLU = (l >> u >> return SuffixUL)
     suffixL  = l >> return SuffixL
     suffixU  = u >> return SuffixU 

plus :: Lexer QToken
plus = do
    char '+'
    d <- optional (csDouble <|> csIntegral)
    case d of
        Nothing  -> return $ QOperator "+"
        (Just x) -> return x

minus :: Lexer QToken
minus = do
    char '-'
    d <- optional (csDouble <|> csIntegral)
    case d of
       (Just (QDouble d s))     -> return $ QDouble (-1*d) s
       (Just (QIntegral i sfx)) -> return $ QIntegral (-1*i) sfx
       Nothing                  -> return $ QOperator "-"

escapedChar :: Lexer Char
escapedChar = do
    char '\\'
    c <- matches (`elem` "'\"\\0abfnrtvxuU")
    case c of
      'n'  -> return '\n'
      'a'  -> return '\a'
      'r'  -> return '\r'
      'v'  -> return '\v'
      't'  -> return '\t'
      '0'  -> return '\0'
      '\''  -> return c
      '\"' -> return c
      '\\' -> return c
      _   -> do { v <- hexDigits;
                  return . chr . read $ "0x" ++ v} 

charLiteral :: Lexer QToken
charLiteral = do
    char '\''
    c <- (escapedChar <|> anyChar)
    char '\''
    return $ QCharLiteral c

strLiteral :: Lexer QToken
strLiteral = do
    char '"'
    cs <- many (escapedChar <|> (matches $ \x -> x /= '"'))
    char '"'
    return $ QStrLiteral cs

csIdent :: Lexer QToken
csIdent = do
    c <- anyChar
    cs <- many (matches $ \x -> isAlphaNum x || x == '_' || x == '$' || (ord x > 128))
    return $ QIdent (c:cs)

csOper :: Lexer QToken
csOper = do
    c <- anyChar
    cs <- many (matches $ flip elem "~`!@$%^&*-+=/.,<>:?%")
    return $ QOperator (c:cs)


singleCharToken :: QToken -> Lexer QToken
singleCharToken t = do
     anyChar
     return t

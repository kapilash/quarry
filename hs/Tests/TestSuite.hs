module Main where

import Text.QToken
import Text.QLexer
import Test.QuickCheck
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Char

newLinesGen :: Gen LexInput
newLinesGen = do
   l <- elements [0..100]
   c <- elements [0..72]
   nl <- elements ["\n","\r\n"]
   return $ LexInput nl l c



decimalDigitGen :: Gen Char
decimalDigitGen = elements "0123456789"

hexaDigitGen = elements "0123456789ABCDEFabcdef"

octaDigitGen = elements "01234567"

binaryDigitGen = elements "01"

decimalGen = listOf1 (elements "0123456789")

octalGen = do
  n <- listOf1 (elements "01234567")
  pref <- elements ['o','O']
  return $ '0':pref:n
  
binaryGen = do
  n <- listOf1 (elements "01")
  p <- elements ['b','B']
  return $ '0':p:n
  
hexGen = do
  n <- listOf1 (elements "0123456789ABCDEFabcdef")
  pref <- elements ['x','X']
  return $ '0':pref:n


remainCheckProp :: LexInput -> Bool
remainCheckProp inp@(LexInput init line col) =
  case (unLex csToken) inp of
   (Left _, LexInput t r c)  -> (filter isDigit t) == []
   (Right i, LexInput x l1 c1) -> x== "REMAINING"

instance Arbitrary LexInput   where
  arbitrary = numberGen

numberGen = oneof [integralGen, doubleGen]
            
integralGen :: Gen LexInput
integralGen = do
  l <- elements [0..100]
  c <- elements [0..72]
  digits <- oneof [decimalGen, octalGen, binaryGen, hexGen]
  suffixes <- elements ["","uLL", "Ull", "ULL","ull","ul","UL","uL","Lu","LLU","llu","L","l","u", "lL","Ll","ll","U"]
  return $ LexInput (digits ++ suffixes ++  "REMAINING") l c

expSuffix :: Gen String
expSuffix = do
  e <- elements ["e","E","E+","e-","E-","e+"]
  digits <- decimalGen
  return $ e ++ digits
  
doubleGen :: Gen LexInput
doubleGen = do
  l <- elements [0..100]
  c <- elements [0..100]
  d1 <- decimalGen
  d2 <- decimalGen
  exp <- oneof [expSuffix, return ""]
  suffix <- elements ["", "l", "F","f","d","D","m", "M"]
  return $ LexInput (d1 ++ "." ++ d2 ++ exp ++ suffix ++  "REMAINING") l c
         
main = defaultMain tests

runSingleInput :: (Show a) => Lexer a -> LexInput -> IO ()
runSingleInput lexr inp@(LexInput i l c) =
    case (unLex lexr) inp of
      (Left s, LexInput t _ _)  -> do { print "fail" ;putStrLn s; putStrLn t; return ()}
      (Right a, LexInput r l c) -> do
                            print a
                            putStrLn r
                            print (l,c)

tests :: [TF.Test]
tests = [
        testGroup "QuickCheck numbers" [
                testProperty "numbers"        remainCheckProp
                ]
        ]

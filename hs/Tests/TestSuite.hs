module Main where

import Text.QToken
import Text.QLexer
import Test.QuickCheck
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)


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

decimalGen = listOf (elements "0123456789")

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
  case (unLex integral) inp of
   (Left _, LexInput t r c)  -> (t == "REMAINING")
   (Right i, LexInput x l1 c1) -> x== "REMAINING"

instance Arbitrary LexInput   where
  arbitrary = integralGen

integralGen :: Gen LexInput
integralGen = do
  l <- elements [0..100]
  c <- elements [0..72]
  digits <- oneof [decimalGen, octalGen, binaryGen, hexGen]
  return $ LexInput (digits ++ "REMAINING") l c

main = defaultMain tests


tests :: [TF.Test]
tests = [
        testGroup "QuickCheck integral" [
                testProperty "readIntegers"        remainCheckProp
                ]
        ]

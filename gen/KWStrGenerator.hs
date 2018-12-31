{-
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of Hemanth Kapila may NOT be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
module Main where

import qualified Data.List as Lst
import Data.Char(ord)
import System.Environment(getArgs)

data KWIndex = KWIndex
     { asciiCode  :: Int,
       concIndex  :: Int,
       kwlenIndex :: Int,
       numWords   :: Int
     }
     deriving (Eq,Show)

data KWIndices = KWIndices [KWIndex] Int Int
      deriving (Show)


initKWIndices = KWIndices [] 0 0

clubKW :: KWIndices -> String -> KWIndices
clubKW k "" = k
clubKW (KWIndices [] _ _) str@(f:_) = KWIndices [KWIndex (ord f) 0 0 1] 1 (length str)
clubKW (KWIndices (findx:rindices) grpCount concatLen) str@(f:_) =
       if ord f == (asciiCode findx) 
              then KWIndices 
                   ((KWIndex (ord f) (concIndex findx) (kwlenIndex findx) (1 + (numWords findx))):rindices) 
                   (grpCount + 1)
                   (length str + concatLen)
              else KWIndices 
                   ((KWIndex (ord f) concatLen grpCount 1):findx:rindices)
                   (grpCount + 1)
                   (length str + concatLen) 

kwsToIndexTable = foldl clubKW initKWIndices

newtype Blocks = Blocks ([[String]])
                 deriving (Show)

addBlock :: Blocks -> [String] -> Blocks
addBlock (Blocks lists) list = Blocks (list:lists)


initBlock :: String -> Blocks
initBlock str = Blocks [ 
  ["#include \"quarry_internal.h\"",
   "#include <stdlib.h>",
   "#include <stdio.h>",
   "",
   "",
   "qu_KWTablePtr quarry_util_keywordTable" ++ str ++ "(){",
   "    unsigned char *concatenated = NULL;",
   "    int *lengths = NULL;",
   "    int index = 0;",
   "    qu_KWTablePtr kwTablePtr = NULL;",
   "    qu_KWsplPtr kwStrArr = NULL;",
   ""
  ]]


sizeDeclAndInitArrs :: KWIndices -> [String]
sizeDeclAndInitArrs (KWIndices lst grpCount conclen) = [
  "    int keywordCount = " ++ (show $ length lst) ++";",
  "    int concatenatedLength = " ++ (show conclen) ++ ";",
  "    int kwIndex = 0;",
  "", 
  "    kwTablePtr = (qu_KWTablePtr)malloc(sizeof(qu_KWTable));",
  "    concatenated = (char *)malloc("++(show conclen)++" * sizeof(unsigned char));",
  "    lengths = (int*)malloc(" ++ (show grpCount) ++ " * sizeof(int));",
  "    kwStrArr = (qu_KWsplPtr)malloc(128 * sizeof(qu_KWspl));",
  "    kwTablePtr->kwIndices = kwStrArr;",
  "    kwTablePtr->word = concatenated;kwTablePtr->concatLength = concatenatedLength;",
  "    kwTablePtr->indices = lengths;kwTablePtr->kwCount = keywordCount;",
  "",
  "    for(index = 0; index<128;index++){",
  "        kwStrArr[index].word = NULL;",
  "        kwStrArr[index].indices = NULL;",
  "        kwStrArr[index].wordCount = 0;",
  "    }"
 ]

kwIndexToCode :: KWIndex -> [String]
kwIndexToCode (KWIndex ac ci li num) = [
              "    kwStrArr["++(show ac)++"].word = &(concatenated["++(show ci)++"]);",
              "    kwStrArr["++(show ac)++"].indices  = &(lengths["++(show li)++"]);",
              "    kwStrArr["++(show ac)++"].wordCount =  "++(show num)++";",
              "",
              ""
              ]

kwIndicesToCode :: KWIndices -> [String]
kwIndicesToCode (KWIndices lst _ _ ) = Lst.concatMap kwIndexToCode lst 

fillConcArr :: [String] -> [String]
fillConcArr strs = map fillConc (zip [0,1..] (Lst.concat strs)) where
           fillConc (i,c) = "    concatenated[" ++ (show $ i) ++"] = "++(show $ ord c)++";"

fillLengths :: [String] -> [String]
fillLengths strs = map fillLen (zip [0,1..] (map length strs))
         where
            fillLen (i,j) = "    lengths[" ++ (show i) ++ "] = "++(show j)++";"



ending :: [String]
ending = [
  "    for(index = 0; index<128;index++){",     
  "        kwStrArr[index].beginKWId = kwIndex;",
  "        kwIndex = kwIndex + kwStrArr[index].wordCount;",
  "    }",
  "    return kwTablePtr;",
  "}"]

testCKW = ["abstract","assert","boolean","break","byte","case","catch","char","class","const","continue",
          "default","do"]


kwInputToCBs :: [String] -> [[String]]
kwInputToCBs strs =
  [sizeDeclAndInitArrs kwis,
   fillConcArr strs,
   fillLengths strs,
   kwIndicesToCode kwis,
   ending]
 where kwis =  kwsToIndexTable strs

kwToBlocks :: String -> [String] -> Blocks
kwToBlocks name strs = foldl addBlock (initBlock name) (kwInputToCBs  $ Lst.sort strs)


printBlock :: Blocks -> IO ()
printBlock (Blocks lst) = mapM_ putStrLn $ concat $ reverse lst

genCode :: FilePath -> IO ()
genCode f = do rf <- readFile f
               putStrLn "/* generating code */"
               printBlock $ kwToBlocks f (lines rf)
               genKeyFile f (lines rf)


genKeyFile f strs = writeFile (f ++ ".key") (unlines $ (asciiList strs) : ( map show $ zip [0,1..] (Lst.sort strs)))

asciiList = show . Lst.sort . map ord . Lst.nub . concat . Lst.sort

main = do args <- getArgs
          case args of
           [] -> putStrLn "No input"
           _  -> mapM_ genCode args
  

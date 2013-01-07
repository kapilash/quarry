{-
Copyright (c) 2013, Hemanth Kapila
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
The name of Hemanth Kapil may NOT be used to endorse or promote products derived from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
module Main where

import qualified Data.List as Lst
import Data.Char(ord)
import System.Environment(getArgs)


posTestCode :: String -> IO ()
posTestCode str = do 
    putStrLn $ "    currKWsize = strlen(\""++ str ++"\");"
    putStrLn $ "    memcpy(input,\""++ str ++ "\",currKWsize);"
    putStrLn $ "    retval = quarry_util_isKeyword(kwTable, input, currKWsize );"
    putStrLn $ "    fail_unless((retval == 1),\"" ++ str ++ " is a keyword\");"
 

genTestCode :: FilePath -> IO ()
genTestCode f = do rf <- readFile f
                   mapM_ posTestCode (lines rf)
  
main = do args <- getArgs
          case args of
           [f] -> genTestCode f
           _ -> putStrLn "No input"


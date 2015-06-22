module Main where

import Language.Tokenizer.Internal
import System.Environment (getArgs)


main = do
  args <- getArgs
  case args of
   [f]  -> testFile f
   _    -> putStrLn "Need One File"



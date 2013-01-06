module Main where

import qualified Data.List as Lst
import Data.Char(ord)
import System.Environment(getArgs)


negTestCode :: String -> IO ()
negTestCode str = do 
    putStrLn $ "    currKWsize = strlen(\""++ str ++"\");"
    putStrLn $ "    memcpy(input,\""++ str ++ "\",currKWsize);"
    putStrLn $ "    retval = quarry_util_isKeyword(kwTable, input, currKWsize );"
    putStrLn $ "    fail_unless((retval == 0),\"" ++ str ++ " is not a keyword\");"
 

genTestCode :: FilePath -> IO ()
genTestCode f = do rf <- readFile f
                   mapM_ negTestCode (lines rf)

main = do args <- getArgs
          case args of
           [f] -> genTestCode f
           _ -> putStrLn "No input"


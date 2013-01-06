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


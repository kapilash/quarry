module Main where

import Language.Tokenizer.Internal
import System.Environment (getArgs)
import qualified Data.Iteratee as I
import System.TimeIt    

isSpaceToken :: NativeToken -> Bool
isSpaceToken (NToken _ _ i _ _) = i == 19

isCommentToken :: NativeToken -> Bool
isCommentToken (NToken _ _ i _ _) = i == 18

tokenCount :: (Monad m) => I.Iteratee [NativeToken] m Int
tokenCount = I.length

spaceAndComments :: I.Iteratee [NativeToken] IO (Int,Int,Int)
spaceAndComments = I.zip3 (I.joinI $ I.filter isSpaceToken tokenCount) (I.joinI $ I.filter isCommentToken tokenCount) tokenCount

printCounts:: FilePath -> IO ()
printCounts f = do
  (c,b,d) <- timeIt (fileDriver 100 f qJava spaceAndComments)
  putStrLn $ "Comments " ++ (show b)
  putStrLn $ "Spaces " ++ (show c)
  putStrLn $ "tokens " ++ (show d)           


printTokens :: FilePath -> IO ()
printTokens f =   timeIt (fileDriver 100 f qJava (I.mapM_ print))

main :: IO()
main = do
  args <- getArgs
  case args of
   [f]  -> printCounts f
   [f,_] -> printTokens f
   _    -> putStrLn "Need One File"



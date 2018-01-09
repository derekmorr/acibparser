module Main where

import           Data.Csv           (encode)
import           Lib
import           System.Environment
import           System.IO
import           Text.Parsec

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      case (runParser showQParser () filename contents) of
        Left err    -> print err
        Right showq -> print $ encode [jobSummary showq]
      -- encode $ jobSummary showq
    _ -> putStrLn "Must specify a filename"

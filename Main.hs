module Main where

import Scheme48.Parsers
import System.Environment

main :: IO ()
main = do
  args <-  getArgs
  putStrLn $ readExpr $ head args

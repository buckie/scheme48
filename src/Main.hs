module Main where

import Scheme48.Parsers (readExpr)
import System.Environment

main :: IO ()
main = do
  args <-  getArgs
  putStrLn $ readExpr $ head args

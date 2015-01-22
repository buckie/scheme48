module Main where

import Scheme48.Eval
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

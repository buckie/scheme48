module Main where

import Scheme48.REPL
import System.Environment

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args

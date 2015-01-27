module Main where

import Scheme48.REPL
import System.Environment

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> runOne $ head args
               _ -> putStrLn "Program takes only 0 or 1 argument"

module Main where

import Control.Monad (liftM)

import Scheme48.Eval (readExpr, eval)
import Scheme48.Error (trapError, extractValue)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled

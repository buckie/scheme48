module Main where

import System.Environment

main = do
    args <- getArgs
    putStrLn ("Hello, " ++ args !! 0)

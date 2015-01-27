
module Scheme48.REPL (
  runRepl,
  evalAndPrint) where

import System.IO
import Control.Monad

import Scheme48.Eval
import Scheme48.Error

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Monad m => String -> m String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (t -> Bool) -> m t -> (t -> m a) -> m ()
until_ pre prompt action = do
  result <- prompt
  if pre result
  then return ()
  else action result >> until_ pre prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "LISP>>> ") evalAndPrint

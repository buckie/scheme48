
module Scheme48.REPL (
  runRepl,
  runOne) where

import System.IO
import Control.Monad

import Scheme48.Eval
import Scheme48.Env

-- REPL code --

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (t -> Bool) -> m t -> (t -> m a) -> m ()
until_ pre prompt action = do
  result <- prompt
  unless (pre result) $ action result >> until_ pre prompt action

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "LISP>>> ") . evalAndPrint

{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Scheme48.Eval (readExpr, eval) where

import Control.Monad.Except

import Scheme48.Error (LispError(..), ThrowsError)
import Scheme48.Types (LispVal(..))
import Scheme48.Parsers (parseExprs)
import Scheme48.StdLib (primitives, eqv)
import Scheme48.REPL hiding (runREPL, evalAndPrint)

-- Evaluation --

eval :: LispVal -> IOThrowsError LispVal
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", cond, t, f]) =
                     do result <- eval cond
                        case result of
                             Bool False -> eval f
                             Bool True -> eval t
                             _ -> throwError $ TypeMismatch "bool" cond
eval v@(List (Atom "cond" : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no true clauses found in cond expression: " v
  else case head clauses of
            List [Atom "else", expr] -> eval expr
            List [test, expr] -> eval $ List [Atom "if", test, expr, List (Atom "cond" : tail clauses)]
            _ -> throwError $ BadSpecialForm "maleformed cond expressioni: " v
eval v@(List (Atom "case" : key : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no true clauses or else clause in case expression:" v
  else case head clauses of
            List (Atom "else": exprs) -> mapM eval exprs >>= return . last
            List ((List datums): exprs) -> do
              result <- eval key
              equality <- mapM (\x -> eqv [result, x]) datums
              if Bool True `elem` equality
              then mapM eval exprs >>= return . last
              else eval $ List (Atom "case" : key : tail clauses)
            _ -> throwError $ BadSpecialForm "maleformed case expression: " v
eval env (List (Atom func:args)) = mapM (eval env) args >>= liftThrows . apply func
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (Atom id) = getVar env id
eval env v@(String _) = return v
eval env v@(Number _) = return v
eval env v@(Bool _) = return v
eval env v@(Character _) = return v
eval env v@(Float _) = return v
eval env v@(Atom _) = return v
eval env v@(Ratio _) = return v
eval env v@(Complex _) = return v
eval env v@(Vector _) = return v
eval env v@(DottedList _ _) = return v
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply fn args = maybe (throwError $ NotFunction "Unrecognized primative function args" fn)
                      ($ args)
                      (lookup fn primitives)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parseExprs input of
    Left err -> throwError $ Parser err
    Right val -> return val



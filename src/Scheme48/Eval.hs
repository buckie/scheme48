{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Scheme48.Eval (readExpr, eval) where

import Scheme48.Error (LispError(..), ThrowsError)
import Scheme48.Types (LispVal(..))
import Scheme48.Parsers (parseExprs)
import Scheme48.StdLib (primitives, eqv)

import Control.Monad.Except

-- Evaluation --

eval :: LispVal -> ThrowsError LispVal
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
eval (List (Atom func:args)) = mapM eval args >>= apply func
eval v@(String _) = return v
eval v@(Number _) = return v
eval v@(Bool _) = return v
eval v@(Character _) = return v
eval v@(Float _) = return v
eval v@(Atom _) = return v
eval v@(Ratio _) = return v
eval v@(Complex _) = return v
eval v@(Vector _) = return v
eval v@(DottedList _ _) = return v
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply fn args = maybe (throwError $ NotFunction "Unrecognized primative function args" fn)
                      ($ args)
                      (lookup fn primitives)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parseExprs input of
    Left err -> throwError $ Parser err
    Right val -> return val



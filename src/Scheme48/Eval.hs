{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Scheme48.Eval where

import Scheme48.Types (LispVal(..))
import Scheme48.Parsers (parseExprs)

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval val = val

readExpr :: String -> LispVal
readExpr input = case parseExprs input of
    Left err -> String $ "No Match: " ++ show err
    Right val -> val

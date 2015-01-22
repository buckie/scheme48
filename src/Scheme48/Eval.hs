{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Scheme48.Eval where

import Scheme48.Types (LispVal(..))
import Scheme48.Parsers (parseExprs)

-- Evaluation --

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List (Atom func:args)) = apply func $ map eval args
eval val = val

apply :: String -> [LispVal] -> LispVal
apply fn args = maybe (Bool False) ($ args) $ lookup fn primitives

readExpr :: String -> LispVal
readExpr input = case parseExprs input of
    Left err -> String $ "No Match: " ++ show err
    Right val -> val

-- Primative Operations --

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("bool?",unaryOp boolp),
              ("symbol?",unaryOp symbolp),
              ("number?",unaryOp numberp),
              ("list?",unaryOp listp),
              ("string?",unaryOp stringp),
              ("vector?",unaryOp vectorp),
              ("dottedlist?",unaryOp dottedListp),
              ("character?",unaryOp characterp),
              ("float?",unaryOp floatp),
              ("ratio",unaryOp ratiop),
              ("complex?",unaryOp complexp),
              ("string->symbol", unaryOp stringToSymbol),
              ("symbol->string", unaryOp symbolToString)
              ]

-- Numeric Operations --

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                            if null parsed
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

-- Type Check Operations --

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v
unaryOp _ _ = error "incorrect application of unary operator"


boolp :: LispVal -> LispVal
boolp (Bool _) = Bool True
boolp _ = Bool False

symbolp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _ = Bool False

numberp :: LispVal -> LispVal
numberp (Number _) = Bool True
numberp _ = Bool False

listp :: LispVal -> LispVal
listp (List _) = Bool True
listp _ = Bool False

stringp :: LispVal -> LispVal
stringp (String _) = Bool True
stringp _ = Bool False

vectorp :: LispVal -> LispVal
vectorp (Vector _) = Bool True
vectorp _ = Bool False

dottedListp :: LispVal -> LispVal
dottedListp (DottedList _ _) = Bool True
dottedListp _ = Bool False

characterp :: LispVal -> LispVal
characterp (Character _) = Bool True
characterp _ = Bool False

floatp :: LispVal -> LispVal
floatp (Float _) = Bool True
floatp _ = Bool False

ratiop :: LispVal -> LispVal
ratiop (Ratio _) = Bool True
ratiop _ = Bool False

complexp :: LispVal -> LispVal
complexp (Complex _) = Bool True
complexp _ = Bool False

-- Symbol Conversion Operations --

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String s) = Atom s
stringToSymbol _ = Atom ""

symbolToString :: LispVal -> LispVal
symbolToString (Atom a) = String a
symbolToString _ = String ""

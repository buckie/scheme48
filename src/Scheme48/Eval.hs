{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Scheme48.Eval where

import Scheme48.Error (LispError(..), ThrowsError)
import Scheme48.Types (LispVal(..))
import Scheme48.Parsers (parseExprs)

import Control.Monad.Except

-- Evaluation --

eval :: LispVal -> ThrowsError LispVal
eval (List [Atom "quote", val]) = return val
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

-- Primative Operations --

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

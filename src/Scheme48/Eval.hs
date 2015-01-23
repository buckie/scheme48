{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Scheme48.Eval where

import Scheme48.Error (LispError(..), ThrowsError)
import Scheme48.Types (LispVal(..))
import Scheme48.Parsers (parseExprs)

import Control.Monad.Except

-- Evaluation --

eval :: LispVal -> ThrowsError LispVal
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", cond, t, f]) =
                     do result <- eval cond
                        case result of
                             Bool False -> eval f
                             _ -> eval t
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
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
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

-- Unpacking Functions --

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                            if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr (Character s) = return $ show s
unpackStr (Float s) = return $ show s
unpackStr (Ratio s) = return $ show s
unpackStr (Complex s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- Numeric Operations --

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ []      = throwError $ NumArgs 2 []
numericBinop _ s@[_]   = throwError $ NumArgs 2 s
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

-- Type Check Operations --

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v
unaryOp _ [] = throwError $ NumArgs 1 []
unaryOp _ _ = throwError $ Default "Incorrect application of unary operator"

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

-- Boolean Binary Operations --

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right


numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

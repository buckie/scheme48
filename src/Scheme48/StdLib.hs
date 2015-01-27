{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Scheme48.StdLib (
  primitives,
  eqv
  ) where

import Scheme48.Error (LispError(..), ThrowsError)
import Scheme48.Types (LispVal(..))

import Control.Monad.Except

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
              ("symbol->string", unaryOp symbolToString),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _ )]         = return x
car [DottedList (x : _ ) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x:xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgs                  = throwError $ NumArgs 2 badArgs

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [xs@(List _), ys@(List _)] = eqvList eqv [xs, ys]
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [xs@(List _), ys@(List _)] = eqvList equal [xs, ys]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

eqvList :: Monad m => ([LispVal] -> Either t LispVal) -> [LispVal] -> m LispVal
eqvList fn [(List xs), (List ys)] =
    return $ Bool $ (length xs == length ys) && (all eqvPair $ zip xs ys)
      where eqvPair (x, y) = case fn [x, y] of
                                Left _ -> False
                                Right (Bool v) -> v
                                _ -> error "unreachable"

eqvList _ _ = error "implementation error!"

-- Unpacking Functions --

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a1 a2 (AnyUnpacker unpacker) = do
          unpacked1 <- unpacker a1
          unpacked2 <- unpacker a2
          return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

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

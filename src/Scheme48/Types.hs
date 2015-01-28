{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Scheme48.Types (
  LispVal(..),
  LispError(..),
  PrimFunc(..),
  Env,
  ThrowsError,
  IOThrowsError,
  unwordsList,
  makeFunc,
  makeNormalFunc,
  makeVarArgs) where

import Numeric ()
import Data.IORef
import Data.Ratio ()
import Data.Complex
import Data.Array
import Control.Monad.Except

import Text.ParserCombinators.Parsec

data PrimFunc = PrimFunc ([LispVal] -> ThrowsError LispVal)

instance Eq PrimFunc where
   _ ==  _ = False
   _ /=  _ = True

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Vector (Array Int LispVal)
             | Func { params :: [String]
                    , vararg :: Maybe String
                    , body :: [LispVal]
                    , closure :: Env}
             | PrimitiveFunc PrimFunc
             deriving (Eq)

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (Atom name)  = name
showVal (String c)   = "\"" ++ c ++ "\""
showVal (Number c)   = show c
showVal (Bool True)  = "#t"
showVal (Bool False) = "#f"
showVal (List c) = "(" ++ unwordsList c ++ ")"
showVal (DottedList x xs) = "(" ++ unwordsList x ++ " . " ++ showVal xs ++ ")"
showVal (Character c) = show c
showVal (Float n) = show n
showVal (Ratio r) = show r
showVal (Complex c) = (show $ realPart c) ++ " + " ++ (show $ imagPart c) ++ "i"
showVal (Vector v) = "#(" ++ unwordsList (elems v) ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = _, closure = _}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs' env' params' body' = return $ Func (map showVal params') varargs' body' env'

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- Errors --

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
  show = showError

showError :: LispError -> String
showError (UnboundVar msg varname)  = msg ++ ": " ++ varname
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func)    = msg ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default s) = "Default Error: " ++ show s

type ThrowsError = Either LispError

-- Env --

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ExceptT LispError IO


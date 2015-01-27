{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Scheme48.Types (LispVal(..), unwordsList) where

import Data.Ratio ()
import Data.Complex
import Data.Array
import Numeric ()

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


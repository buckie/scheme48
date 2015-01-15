{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Control.Monad
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- Strings --

escapedChars :: Parser Char
escapedChars = do 
                _ <- char '\\' 
                x <- oneOf "\\\"nrt"
                return $ case x of 
                            '\\' -> x
                            '"'  -> x
                            'n'  -> '\n'
                            'r'  -> '\r'
                            't'  -> '\t'
                            _    -> error "impossible"


parseString :: Parser LispVal
parseString = do
                _ <- char '"'
                x <- many (noneOf "\"")
                _ <- char '"'
                return $ String x

parseCharacter :: Parser LispVal
parseCharacter = do 
                    try $ string "#\\"
                    x <- try (string "newline" <|> string "space") 
                        <|> do { v <- anyChar; notFollowedBy alphaNum; return [v] }
                    return $ Character $ case x of
                            "newline" -> '\n'
                            "space"   -> ' '
                            _ -> head x

-- Atoms --

parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ Atom atom

--- Numbers ---

parseNumber :: Parser LispVal
parseNumber = parseDigit1 <|> parseDigit2 <|> parseHex <|> parseOct <|> parseBin

parseDigit1 :: Parser LispVal
parseDigit1 = liftM (Number . read) (many1 digit)

parseDigit2 :: Parser LispVal
parseDigit2 = string "#d" >> parseDigit1

parseHex :: Parser LispVal
parseHex =  try $ liftM (Number . hex2dig) (string "#x" >> many1 hexDigit)

hex2dig :: (Num a, Eq a) => String -> a
hex2dig = fst . head . readHex

parseOct :: Parser LispVal
parseOct = try $ liftM (Number . oct2dig) (string "#o" >> many1 octDigit)

oct2dig :: (Num a, Eq a) => String -> a
oct2dig = fst . head . readOct

parseBin :: Parser LispVal
parseBin = try $ liftM (Number . bin2dig) (string "#b" >> many1 (oneOf "01"))

bin2dig :: String -> Integer
bin2dig = bin2dig' 0

bin2dig' :: Num a => a -> String -> a
bin2dig' d ""     = d
bin2dig' d (x:xs) = let
                        n = case x of 
                                '0' -> 0
                                '1' -> 1
                                _   -> error "Impossible"
                        o = 2 * d + n
                    in bin2dig' o xs

-- Booleans --

parseBool :: Parser LispVal
parseBool = char '#' >> ((char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False)))

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseBool
        <|> try parseNumber
        <|> try parseCharacter

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No Match: " ++ show err
    Right val -> "Found Value: " ++ show val

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr $ head args

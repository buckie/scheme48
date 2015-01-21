{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Scheme48.Parsers where

import Scheme48.Types

import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array
import Text.ParserCombinators.Parsec hiding (spaces)

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
parseString = try $ do
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
parseAtom = try $ do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ Atom atom

--- Numbers ---

parseNumber :: Parser LispVal
parseNumber = try parseComplex
           <|> try parseRatio
           <|> try parseFloat
           <|> try parseDigit1
           <|> try parseDigit2
           <|> try parseHex
           <|> try parseOct
           <|> try parseBin

parseDigit1 :: Parser LispVal
parseDigit1 = try $ liftM (Number . read) (many1 digit)

parseDigit2 :: Parser LispVal
parseDigit2 = try $ string "#d" >> parseDigit1

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

-- Floats --

parseFloat :: Parser LispVal
parseFloat = try $ do
  i <- many1 digit
  _ <- char '.'
  d <- many1 digit
  return $ Float $ fst . head $ readFloat $ i ++ "." ++ d

-- Ratios --

parseRatio :: Parser LispVal
parseRatio = try $ do
  n <- many1 digit
  _ <- char '/'
  d <- many1 digit
  return $ Ratio $ (read n) % (read d)

-- Complex --

parseComplex :: Parser LispVal
parseComplex = try $ do
  x <- (try parseFloat <|> parseDigit1)
  _ <- char '+'
  y <- (try parseFloat <|> parseDigit1)
  _ <- char 'i'
  return $ Complex $ toDouble x :+ toDouble y

toDouble :: LispVal -> Double
toDouble (Float f) = f
toDouble (Number n) = fromIntegral n
toDouble e = error $ "wrong type: " ++ show e

-- Booleans --

parseBool :: Parser LispVal
parseBool = try $ char '#' >>
  ((char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False)))

-- Lists --

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = try $ do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseLists :: Parser LispVal
parseLists = try $ do
  _ <- char '('
  x <- try parseList <|> parseDottedList
  _ <- char ')'
  return x

-- quasiquote --

parseQuasi :: Parser LispVal
parseQuasi = try $ liftM (\x -> List [Atom "quasiquote", x]) (char '`' >> parseExpr)

parseUnQuote :: Parser LispVal
parseUnQuote = try $ liftM (\x -> List [Atom "unquote", x]) (char ',' >> parseExpr)

-- Vector --

parseVector :: Parser LispVal
parseVector = try $ do
  _ <- string "#("
  v <- sepBy parseExpr spaces
  _ <- char ')'
  return $ Vector $ listArray (0, (length v - 1)) v

-- Core Parser --

parseExpr :: Parser LispVal
parseExpr = parseNumber
        <|> parseBool
        <|> parseCharacter
        <|> parseLists
        <|> parseQuasi
        <|> parseUnQuote
        <|> parseVector
        <|> parseString
        <|> parseAtom
        <|> parseQuoted

parseExprs :: String -> Either ParseError LispVal
parseExprs = parse parseExpr "lisp"

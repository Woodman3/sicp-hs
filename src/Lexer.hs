module Lexer
    ( Token,lexer
    ) where

import Data.Char( isDigit )

data Token = Int Int | Mul | Sub | Add | Div | LPar | RPar
    deriving (Show)

lexer :: String -> [Token]
lexer s = case lexeme s of 
    Nothing -> []
    Just (t,xs) -> t: lexer(xs)

lexeme :: String -> Maybe ( Token , String)
lexeme [] = Nothing
lexeme (x : xs)
    | x == '*' = Just (Mul, xs)
    | x == '/' = Just (Div, xs)
    | x == '+' = Just (Add, xs)
    | x == '-' = Just (Sub, xs)
    | x == '(' = Just (LPar, xs)
    | x == ')' = Just (RPar, xs)
    | x == ' '|| x == '\n' = lexeme xs
    | isDigit x = toNumberToken (x : xs)
    | otherwise = Nothing

toNumberToken :: String -> Maybe ( Token , String)
toNumberToken [] = Nothing
toNumberToken xs = let (num, rest) = span isDigit xs
                   in Just (Int (read num), rest)
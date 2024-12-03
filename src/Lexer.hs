module Lexer
    ( lexer
    ) where

import Data.Char( isDigit )
import Token

data Proc = Op Op
data Args = Int Int | AST AST
data AST = AST Proc [Args]

lexer :: String -> [Token]
lexer s = case lexeme s of 
    Nothing -> []
    Just (t,xs) -> t: lexer(xs)

lexeme :: String -> Maybe ( Token , String)
lexeme [] = Nothing
lexeme (x : xs)
    | x == '*' = Just (Op Mul, xs)
    | x == '/' = Just (Op Div, xs)
    | x == '+' = Just (Op Add, xs)
    | x == '-' = Just (Op Sub, xs)
    | x == '(' = Just (LPar, xs)
    | x == ')' = Just (RPar, xs)
    | x == ' '|| x == '\n' = lexeme xs
    | isDigit x = toNumberToken (x : xs)
    | otherwise = Nothing

toNumberToken :: String -> Maybe ( Token , String)
toNumberToken [] = Nothing
toNumberToken xs = let (num, rest) = span isDigit xs
                   in Just (Int (read num), rest)

tokenize :: String -> [Token]
tokenize s = case lexeme s of 
    Nothing -> []
    Just (t,xs) -> t: tokenize(xs)

to_ast :: [Token] -> AST
to_ast [] = error "unexpected end of input"
to_ast (x:xs) = case x of
    Op o -> AST (Op o) (to_args xs)
    LPar -> to_ast xs 
    RPar -> to_ast xs
    _ -> error $ "unexpected token in to_ast " ++ show x

to_args :: [Token] -> [Args]
to_args [] = []
to_args (x:xs) = case x of
    Int i -> Int i : to_args xs
    LPar -> AST (to_ast xs) : to_args xs
    Op o -> AST (to_ast (x:xs)) : to_args xs
    RPar -> []
    _ -> error $ "unexpected token in to_args " ++ show x
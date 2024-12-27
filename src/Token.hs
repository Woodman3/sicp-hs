module Token(Token(..),
    Op(..),
    is_op) where

import Parser

data Token = Int Int | Op Op | LPar | RPar
    deriving (Show,Eq)

data Op = Mul | Sub | Add | Div
    deriving (Eq)

instance Show Op where
    show Mul = "*"
    show Sub = "-"
    show Add = "+"
    show Div = "/"

is_op :: Token -> Bool
is_op (Op _) = True
is_op _ = False

-- digit :: Parser Int
-- digit = read <$> some (satisfy isDigit)

-- op :: Parser Op
-- op = read <$> satisfy $ \c-> c == '*' || c == '/' || c == '+' || c == '-'


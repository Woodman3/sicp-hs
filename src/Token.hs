module Token(Token(..),
    Op(..),
    is_op) where

data Token = Int Int | Op Op | LPar | RPar
    deriving (Show,Eq)

data Op = Mul | Sub | Add | Div
    deriving (Show,Eq)

is_op :: Token -> Bool
is_op (Op _) = True
is_op _ = False
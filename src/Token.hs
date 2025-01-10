module Token(
    Atom(..),
    Op(..),
    SExp(..),
    s_exp
) where

import Parser
import Control.Applicative
import Data.Char

data Atom = Int Int | Bool Bool | String String deriving (Show,Eq)

data Op = Mul | Div | Add | Sub deriving (Eq)

instance Show Op where
    show Mul = "*"
    show Div = "/"
    show Add = "+"
    show Sub = "-"

data SExp = Node Op [SExp] | Leaf Atom deriving (Show,Eq)

atom :: Parser Char Atom
atom = foldr1 ( <|>) [
    Int <$> int,
    Bool <$> bool,
    String <$> exp_string
    ]
     

bool :: Parser Char Bool
bool = True <$ string "true" <|> False <$ string "false"

int ::  Parser Char  Int
int = read <$> some (satisfy isDigit)

exp_string :: Parser Char String
exp_string = char '"' *> many (satisfy (/= '"')) <* char '"'

node :: Parser Char SExp
node = do
    char '('
    spaces
    op <- exp_op
    exps <- many (spaces *> s_exp) 
    spaces
    char ')'
    return $ Node op exps

s_exp :: Parser Char SExp
s_exp = node <|> Leaf <$> atom

exp_op :: Parser Char Op
exp_op = to_op <$> ( satisfy $ \c-> c == '*' || c == '/' || c == '+' || c == '-' )
    where to_op '*' = Mul
          to_op '/' = Div
          to_op '+' = Add
          to_op '-' = Sub
          to_op _ = error "unexpected operator"



module Token(
    Atom(..),
    Op(..),
    SExp(..),
    sExp
) where

import Parser
import Control.Applicative
import Data.Char

data Atom = Int Int | Bool Bool | String String deriving (Show,Eq)

data Op = Mul | Div | Add | Sub  deriving (Eq)

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
    String <$> expString
    ]
     

bool :: Parser Char Bool
bool = True <$ string "#t" <|> False <$ string "#f"

int ::  Parser Char  Int
int = read <$> some (satisfy isDigit)

expString :: Parser Char String
expString = char '"' *> many (satisfy (/= '"')) <* char '"'

node :: Parser Char SExp
node = do
    char '('
    spaces
    op <- expOp
    exps <- many (spaces *> sExp) 
    spaces
    char ')'
    return $ Node op exps

sExp :: Parser Char SExp
sExp = node <|> Leaf <$> atom

expOp :: Parser Char Op
expOp = to_op <$> ( satisfy $ \c-> c == '*' || c == '/' || c == '+' || c == '-' )
    where to_op '*' = Mul
          to_op '/' = Div
          to_op '+' = Add
          to_op '-' = Sub
          to_op _ = error "unexpected operator"



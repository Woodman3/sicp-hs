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

newtype Op = Op String deriving (Show,Eq)

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
expOp = Op <$> matchString ["+", "-", "*", "/", 
    "<",">","=","<=",">="]

matchString :: [String] -> Parser Char String
matchString = foldr1 (<|>) . map string


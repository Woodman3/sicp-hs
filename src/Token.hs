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

atom :: Parser Atom
atom = foldr1 ( <|>) [
    Int <$> int,
    Bool <$> bool,
    String <$> expString
    ]


bool :: Parser Bool
bool = True <$ string "#t" <|> False <$ string "#f"

int ::  Parser  Int
int = read <$> some (satisfy isDigit)

expString :: Parser String
expString = char '"' *> many (satisfy (/= '"')) <* char '"'

node :: Parser SExp
node = do
    _ <- char '('
    spaces
    op <- expOp
    exps <- many (spaces *> sExp)
    spaces
    _ <- char ')'
    return $ Node op exps

sExp :: Parser SExp
sExp = node <|> Leaf <$> atom

expOp :: Parser Op
expOp = Op <$> matchString ["+", "-", "*", "/", 
    "<=",">=","<",">","=","and","or","not",
    "if"]

matchString :: [String] -> Parser String
matchString = foldr1 (<|>) . map string


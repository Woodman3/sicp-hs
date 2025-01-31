module Token(
    Atom(..),
    SExp(..),
    sExp
) where

import Parser
import Control.Applicative
import Data.Char

data Atom = Int Int 
    | Bool Bool 
    | String String
    | Op String deriving (Show,Eq)

data SExp = Node [SExp] | Leaf Atom deriving (Show,Eq)

atom :: Parser Atom
atom = foldr1 ( <|>) [
    Int <$> int,
    Bool <$> bool,
    String <$> expString,
    expOp
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
    exps <- many (spaces *> sExp <* spaces)
    _ <- char ')'
    return $ Node exps

sExp :: Parser SExp
sExp = node <|> Leaf <$> atom

expOp :: Parser Atom
expOp = Op <$> matchString ["+", "-", "*", "/", 
    "<=",">=","<",">","=","and","or","not",
    "if","cond","else"]

matchString :: [String] -> Parser String
matchString = foldr1 (<|>) . map string


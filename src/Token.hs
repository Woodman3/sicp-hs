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
    | Op String
    | Var String deriving (Show,Eq)

data SExp = Node [SExp] | Leaf Atom deriving (Show,Eq)

atom :: Parser Atom
atom = do
    spaces
    a <- foldr1 ( <|>) [
        Int <$> int,
        Bool <$> bool,
        String <$> expString,
        expOp,
        expVar
        ]
    spaces
    return a


bool :: Parser Bool
bool = True <$ string "#t" <|> False <$ string "#f"

int ::  Parser  Int
int = read <$> some (satisfy isDigit)

expString :: Parser String
expString = char '"' *> many (satisfy (/= '"')) <* char '"'

node :: Parser SExp
node = do
    spaces
    _ <- char '('
    exps <- many sExp 
    _ <- char ')'
    spaces
    return $ Node exps

sExp :: Parser SExp
sExp = node <|> Leaf <$> atom

expOp :: Parser Atom
expOp = Op <$> matchString ["+", "-", "*", "/", 
    "<=",">=","<",">","=","and","or","not",
    "if","cond","else",
    "begin",
    "define","set1"]

expVar :: Parser Atom
expVar = Var <$> some (satisfy isAlpha)

matchString :: [String] -> Parser String
matchString = foldr1 (<|>) . map string


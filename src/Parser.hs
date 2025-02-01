module Parser(
    Parser(..),
    ParseError(..),
    run,
    satisfy,
    eof,
    char,
    string,
    spaces
) where

import Control.Applicative
import Data.List (nub)
import Data.Char

type Offset = Int

data ParseError i  = ParseError {
    error_pos :: Offset,
    error_type :: ErrorType i
} deriving (Show,Eq)

data ErrorType i
    = UnexpectedEndOfInput
    | Expect i i
    | ExpectEOF i
    | Unexpected i
    | Empty
    deriving (Show,Eq)

newtype Parser a = Parser{ parse :: [Char]->Offset->Either [ ParseError Char ] (a,Offset,[Char]) }

errorToken ::(Char->ErrorType Char) -> (Char->Bool)->Parser Char
errorToken mk_err f = Parser $ \input offset -> case input of
    (x:xs)-> if f x then Right (x,offset+1,xs) else Left [ParseError offset ( mk_err x )]
    []-> Left [ParseError offset UnexpectedEndOfInput]

satisfy :: (Char->Bool)->Parser Char
satisfy = errorToken Unexpected

eof :: Parser ()
eof = Parser $ \input offset -> case input of
    []-> Right ((),offset,[])
    (x:xs)-> Left [ParseError offset ( ExpectEOF x )]

spaces :: Parser ()
spaces = () <$ many (satisfy isSpace)

char :: Char ->Parser Char
char c = errorToken (Expect c) (==c)

string :: [Char] -> Parser [Char]
string [] = return []
string (x:xs) = do
    rx <- char x
    rxs <- string xs
    return (rx:rxs)

run :: Parser a -> [Char] -> Either [ParseError Char] a
run p input = case parse p input 0 of
    Right (a,_,_)-> Right a
    Left e -> Left e

instance Functor Parser where
    fmap f pa = Parser $ \input offset  -> do
        (x,offset',xs) <- parse pa input offset
        return (f x,offset',xs)

instance Applicative Parser where
    pure x = Parser $ \input offset-> Right (x,offset,input)
    pf <*> pa = Parser $ \input offset -> do
        (f,offset',input') <- parse pf input offset
        (a,offset'',input'') <- parse pa input' offset'
        return (f a,offset'',input'')

instance Monad Parser where
    pa >>= f = Parser $ \input offset -> do
        (a,offset',rest) <- parse pa input offset
        parse (f a) rest offset'

instance  Alternative Parser  where
    empty = Parser $ \_ offset -> Left [ParseError offset Empty]
    pa <|> pb = Parser $ \input offset-> case parse pa input offset of
        Right r  -> Right r
        Left e -> case parse pb input offset of
            Right r  -> Right r
            Left e' -> Left $ nub $ e <> e'
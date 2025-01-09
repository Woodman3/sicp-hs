module Parser where

import Control.Applicative
import Data.List (nub)
import Data.Char

type Offset = Int

data Error i  = Error {
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

data Parser i a = Parser{ parse :: [i]->Offset->Either [ Error i ] (a,Offset,[i]) } 

error_token ::(i->ErrorType i) -> (i->Bool)->Parser i i
error_token mk_err f = Parser $ \input offset -> case input of
    (x:xs)-> if f x then Right (x,offset+1,xs) else Left [Error offset ( mk_err x )] 
    []-> Left [Error offset UnexpectedEndOfInput]

satisfy :: (i->Bool)->Parser i i 
satisfy f = error_token (\x -> Unexpected x) f

eof :: Parser i ()
eof = Parser $ \input offset -> case input of
    []-> Right ((),offset,[])
    (x:xs)-> Left [Error offset ( ExpectEOF x )]

spaces :: Parser Char ()
spaces = () <$ many (satisfy isSpace)

char :: Eq i => i ->Parser i i 
char c = error_token (\x -> Expect c x) (==c)

string :: Eq i => [i] -> Parser i [i] 
string [] = return []
string (x:xs) = do
    rx <- char x
    rxs <- string xs
    return (rx:rxs)

run :: Parser i a -> [i] -> Either [Error i] a
run p input = case parse p input 0 of
    Right (a,_,_)-> Right a
    Left e -> Left e

instance Functor ( Parser i )  where
    fmap f pa = Parser $ \input offset  -> do
        (x,offset',xs) <- parse pa input offset
        return (f x,offset',xs) 

instance Applicative (Parser i) where
    pure x = Parser $ \input offset-> Right (x,offset,input) 
    pf <*> pa = Parser $ \input offset -> do
        (f,offset',input') <- parse pf input offset
        (a,offset'',input'') <- parse pa input' offset'
        return (f a,offset'',input'')

instance Monad (Parser i) where
    pa >>= f = Parser $ \input offset -> do
        (a,offset',rest) <- parse pa input offset
        parse (f a) rest offset'

instance ( Eq i ) => Alternative (Parser i ) where
    empty = Parser $ \_ offset -> Left [Error offset Empty] 
    pa <|> pb = Parser $ \input offset-> case parse pa input offset of
        Right r  -> Right r
        Left e -> case parse pb input offset of
            Right r  -> Right r
            Left e' -> Left $ nub $ e <> e' 
module Parser where

import Control.Applicative
import Data.List (nub)

type Offset = Int

data Error i e = Error {
    error_pos :: Offset,
    error_type :: ErrorType i e
} deriving (Show,Eq)

data ErrorType i e 
    = UnexpectedEndOfInput
    | Expect i i
    | ExpectEOF i
    | Unexpected i
    | CustomError e
    | Empty
    deriving (Show,Eq)

data Parser i e a = Parser{ parse :: [i]->Offset->Either [ Error i e ] (a,Offset,[i]) } 

error_token ::(i->ErrorType i e) -> (i->Bool)->Parser i e i
error_token mk_err f = Parser $ \input offset -> case input of
    (x:xs)-> if f x then Right (x,offset+1,xs) else Left [Error offset ( mk_err x )] 
    []-> Left [Error offset UnexpectedEndOfInput]

satisfy :: (i->Bool)->Parser i e i 
satisfy f = error_token (\x -> Unexpected x) f

eof :: Parser i e ()
eof = Parser $ \input offset -> case input of
    []-> Right ((),offset,[])
    (x:xs)-> Left [Error offset ( ExpectEOF x )]

char :: Eq i => i ->Parser i e i 
char c = error_token (\x -> Expect c x) (==c)

string :: Eq i => [i] -> Parser i e [i] 
string [] = return []
string (x:xs) = do
    rx <- char x
    rxs <- string xs
    return (rx:rxs)

instance Functor ( Parser i e )  where
    fmap f pa = Parser $ \input offset  -> do
        (x,offset',xs) <- parse pa input offset
        return (f x,offset',xs) 
    -- fmap f pa = Parser $ \a-> case parse pa a of 
    --     Just(x,xs)-> Just(f x,xs)
    --     Nothing -> Nothing

instance Applicative (Parser i e) where
    pure x = Parser $ \input offset-> Right (x,offset,input) 
    pf <*> pa = Parser $ \input offset -> do
        (f,offset',input') <- parse pf input offset
        (a,offset'',input'') <- parse pa input' offset'
        return (f a,offset'',input'')

instance Monad (Parser i e) where
    pa >>= f = Parser $ \input offset -> do
        (a,offset',rest) <- parse pa input offset
        parse (f a) rest offset'

instance ( Eq i , Eq e ) => Alternative (Parser i e) where
    empty = Parser $ \_ offset -> Left [Error offset Empty] 
    pa <|> pb = Parser $ \input offset-> case parse pa input offset of
        Right r  -> Right r
        Left e -> case parse pb input offset of
            Right r  -> Right r
            Left e' -> Left $ nub $ e <> e' 
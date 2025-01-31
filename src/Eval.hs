module Eval(
    eval
)where

import Token

eval :: SExp -> Atom
eval ( Leaf a ) = a
eval ( Node (x:xs) ) = case eval x of
    Op "if" -> if toBool $ eval $ head xs
        then eval $ xs !! 1
        else eval $ xs !! 2
    Op "cond" -> evalCond xs
    Op o ->  apply o (listOfValues xs)
    _ -> error "invalid expression"
eval _ = error "invalid expression"


evalCond :: [SExp] -> Atom
evalCond [] = error "no true clause in cond"
evalCond (x:xs)= case x of
    Node [Leaf (Op "else"),y] -> eval y
    Node [c,y] -> if toBool $ eval c
        then eval y
        else evalCond xs
    _ -> error "invalid cond clause"

apply :: String -> [Atom] -> Atom
apply = primitiveProcedure

listOfValues :: [SExp] -> [Atom]
listOfValues = map eval

primitiveProcedure :: String -> [Atom] -> Atom
primitiveProcedure "+" args = Int $ foldr (+) 0 $ map (toInt) args -- (+) return just 0 in lisp
primitiveProcedure "-" args = Int $ foldr1 (-) $ map (toInt) args -- (-) return will error in lisp
primitiveProcedure "*" args = Int $ foldr (*) 1 $ map (toInt) args -- (*) return just 1 in lisp
primitiveProcedure "/" args = Int $ foldr1 div $ map (toInt) args -- (/) return will error in lisp
primitiveProcedure "<" args = Bool $  and $ zipWith (<) (map toInt args) (tail $ map toInt args) -- in scheme, (<) return #t (< 1) return #t
primitiveProcedure ">" args = Bool $ and $ zipWith (>) (map toInt args) (tail $ map toInt args)
primitiveProcedure "=" args  = Bool $ and $ zipWith (==) (map toInt args) (tail $ map toInt args)
primitiveProcedure "<=" args = Bool $  and $ zipWith (<=) (map toInt args) (tail $ map toInt args) -- in scheme, (<) return #t (< 1) return #t
primitiveProcedure ">=" args = Bool $ and $ zipWith (>=) (map toInt args) (tail $ map toInt args)
primitiveProcedure "and" args = Bool $ and $ map toBool args
primitiveProcedure "or" args = Bool $ or $ map toBool args
primitiveProcedure "not" [Bool b] = Bool $ not b
primitiveProcedure _ _ = error "unknown operator"


toInt :: Atom -> Int
toInt (Int i) = i
toInt _ = error "not an integer"

toBool :: Atom -> Bool
toBool (Bool b) = b
toBool _ = error "not a boolean"

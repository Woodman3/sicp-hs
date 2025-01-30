module Eval(
    eval    
)where

import Token

eval :: SExp -> Atom
eval ( Leaf a ) = a
eval ( Node o s ) = apply o (listOfValues s)

apply :: Op -> [Atom] -> Atom
apply op args = primitiveProcedure op args

listOfValues :: [SExp] -> [Atom]
listOfValues [] = []
listOfValues (x:xs) = eval x : listOfValues xs

primitiveProcedure :: Op -> [Atom] -> Atom
primitiveProcedure Add args = Int $ foldr (+) 0 $ map (toInt) args -- (+) return just 0 in lisp
primitiveProcedure Sub args = Int $ foldr1 (-) $ map (toInt) args -- (-) return will error in lisp
primitiveProcedure Mul args = Int $ foldr (*) 1 $ map (toInt) args -- (*) return just 1 in lisp
primitiveProcedure Div args = Int $ foldr1 div $ map (toInt) args -- (/) return will error in lisp

toInt :: Atom -> Int
toInt (Int i) = i
toInt _ = error "not an integer"

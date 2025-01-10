module Eval(
    eval    
)where

import Token
import Debug.Trace

eval :: SExp -> Atom
eval ( Leaf a ) = a
eval ( Node o s ) = apply o (list_of_values s)

apply :: Op -> [Atom] -> Atom
apply op args = primitive_procedure op args

list_of_values :: [SExp] -> [Atom]
list_of_values [] = []
list_of_values (x:xs) = eval x : list_of_values xs

primitive_procedure :: Op -> [Atom] -> Atom
primitive_procedure Add args = Int $ foldr (+) 0 $ map (to_int) args -- (+) return just 0 in lisp
primitive_procedure Sub args = Int $ foldr1 (-) $ map (to_int) args -- (-) return will error in lisp
primitive_procedure Mul args = Int $ foldr (*) 1 $ map (to_int) args -- (*) return just 1 in lisp
primitive_procedure Div args = Int $ foldr1 div $ map (to_int) args -- (/) return will error in lisp
    where 
        to_int (Int i) = i
        to_int _ = error "not an integer"


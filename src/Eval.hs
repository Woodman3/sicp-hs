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
primitiveProcedure ( Op "+" ) args = Int $ foldr (+) 0 $ map (toInt) args -- (+) return just 0 in lisp
primitiveProcedure ( Op "-" ) args = Int $ foldr1 (-) $ map (toInt) args -- (-) return will error in lisp
primitiveProcedure (Op "*") args = Int $ foldr (*) 1 $ map (toInt) args -- (*) return just 1 in lisp
primitiveProcedure (Op "/") args = Int $ foldr1 div $ map (toInt) args -- (/) return will error in lisp
primitiveProcedure (Op "<") args = Bool $  and $ zipWith (<) (map toInt args) (tail $ map toInt args) -- in scheme, (<) return #t (< 1) return #t
primitiveProcedure (Op ">") args = Bool $ and $ zipWith (>) (map toInt args) (tail $ map toInt args)
primitiveProcedure (Op "=") args  = Bool $ and $ zipWith (==) (map toInt args) (tail $ map toInt args)
primitiveProcedure (Op "<=") args = Bool $  and $ zipWith (<=) (map toInt args) (tail $ map toInt args) -- in scheme, (<) return #t (< 1) return #t
primitiveProcedure (Op ">=") args = Bool $ and $ zipWith (>=) (map toInt args) (tail $ map toInt args)
primitiveProcedure (Op "and") args = Bool $ and $ map toBool args
primitiveProcedure (Op "or") args = Bool $ or $ map toBool args
primitiveProcedure (Op "not") [Bool b] = Bool $ not b
primitiveProcedure _ _ = error "unknown operator"


toInt :: Atom -> Int
toInt (Int i) = i
toInt _ = error "not an integer"

toBool :: Atom -> Bool
toBool (Bool b) = b
toBool _ = error "not a boolean"

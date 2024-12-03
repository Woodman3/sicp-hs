module Main (main) where

import Lexer
import Token

import Debug.Trace
import System.IO( openFile, hGetContents, hClose, IOMode(ReadMode) )

eval :: [Token] -> [Token]
eval [] = []
eval [x]
    | is_self_evaluating x = [x]
    | otherwise = error $ "Unexpected token: " ++ show x
eval (x:xs) 
    -- | x== LPar = let (a, b) = span (/= RPar) xs 
    --                 in eval a ++ eval (tail b)
    | is_op x = [apply x ( list_of_values xs )]
    | otherwise = x : list_of_values xs

list_of_values :: [Token] -> [Token]
list_of_values [] = []
list_of_values xs = let (a, b) = trace (show xs) ( first_operand 0 [] xs )
                    in trace (" trace  " ++ show a ++ show b )(  eval a ++ list_of_values b )

first_operand :: Int->[Token]->[Token] -> ([Token], [Token])
first_operand 0 l [] = (l, [])
first_operand _ l [] = error $ "unexpected end of input " ++ show l
-- first_operand acc l (RPar:xs)
--     | acc >1 = first_operand (acc-1) (l++[RPar]) xs
--     | acc ==1 = (tail l, xs)
--     | otherwise = error $ "unexpected ) of " ++ show l ++" " ++ show xs
-- first_operand 0 [] (x:xs) = ([x], xs)
first_operand acc l (x:xs)
    | x == RPar
        = if acc > 1 then first_operand (acc-1) (l++[RPar]) xs
        else if acc == 1 then (tail l, xs)
        else error $ "unexpected ) of " ++ show l ++" " ++ show xs
    | x == LPar = first_operand (acc+1) (l++[LPar]) xs
    | otherwise = 
        if acc ==0 then ([x], xs)
        else first_operand acc (l++[x]) xs


is_self_evaluating :: Token -> Bool
is_self_evaluating (Int _) = True
is_self_evaluating _ = False

apply :: Token -> [Token] -> Token
apply (Op Add) (Int x:Int y:[]) = Int (x + y)
apply (Op Sub) (Int x:Int y:[]) = Int (x - y)
apply (Op Mul) (Int x:Int y:[]) = Int (x * y)
apply (Op Div) (Int x:Int y:[]) = Int (x `div` y)
apply a b = error $ "unknown operator in apply " ++ show a ++ " "++ show b

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print contents
    mapM_ print $ lexer contents
    print "eval"
    mapM_ print $ eval $ lexer contents
    hClose handle

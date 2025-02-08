{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Eval(
    eval,
    Env(..),
    IError(..),
    Intrp(..)
)where
import Token

import Data.Map(Map)
import Control.Monad.Except
import Control.Monad.State

newtype Env = Env (Map String SExp) deriving Show
data IError = EvalError String 
    | CondError String
    | ApplyError String
    | PrimitiveError String
    | ToIntError String
    | ToBoolError String
    | UnknownOperatorError String
    deriving Show
newtype Intrp a = Intrp { intrp :: ExceptT IError (State Env) a }
    deriving (Functor, Applicative, Monad, MonadError IError, MonadState Env)

eval :: SExp -> Intrp Atom
eval ( Leaf a ) = return a
eval ( Node (x:xs) ) =eval x >>= \case
    Op "if" -> evalIf xs
    Op "cond" -> evalCond xs
    Op "begin" -> last <$> mapM eval xs
    Op o -> do
        args <- listOfValues xs
        apply o args
    _ -> throwError $ EvalError "invalid expression"
eval _ = throwError $ EvalError "invalid expression"


evalCond :: [SExp] -> Intrp Atom
evalCond [] = throwError $ CondError "no true or else clause" 
evalCond (x:xs)= case x of
    Node [Leaf (Op "else"),y] -> eval y
    Node [c,y] -> do
        cond <- eval c
        if toBool cond
            then eval y
            else evalCond xs
    _ -> throwError $ CondError "invalid clause"

evalIf :: [SExp] -> Intrp Atom
evalIf [c,t,e] = do
    cond <- eval c
    if toBool cond
        then eval t
        else eval e
evalIf _ = throwError $ EvalError "invalid if expression"

apply :: String -> [Atom] -> Intrp Atom
apply o args = return $ primitiveProcedure o args

listOfValues :: [SExp] -> Intrp [Atom]
listOfValues = mapM eval

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

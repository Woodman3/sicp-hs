module Interpreter(evalExpr) where

import Token(sExp,SExp) 
import Parser(parse,ParseError(..))
import Eval(eval,Env(..),IError(..),Intrp(..))

import Data.Map(empty)
import Control.Monad.Except(ExceptT,runExceptT)
import Control.Monad.State(State,runState)

runIntrp :: Intrp a -> Either IError a
runIntrp i =case (runState . runExceptT . intrp) i (Env empty) of
    (Left e,_) -> Left e
    (Right a,_) -> Right a

evalExpr :: String -> Either String String
evalExpr s = case parse sExp s 0 of
    Left p -> Left $ show p
    Right (x,_,_) -> case runIntrp (eval x)  of
        Left (IError e) -> Left e
        Right a -> Right $ show a
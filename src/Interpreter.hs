module Interpreter(evalExpr) where

import Token(sExp) 
import Parser(parse)
import Eval(eval,Env(..),IError(..),Intrp(..))

import Data.Map(empty)
import Control.Monad.Except(runExceptT)
import Control.Monad.State(runState)
import Control.Applicative(some)

runIntrp :: Intrp a -> Either IError a
runIntrp i =case (runState . runExceptT . intrp) i (Env empty) of
    (Left e,_) -> Left e
    (Right a,_) -> Right a

evalExpr ::String -> Either String String
evalExpr s = case parse (some sExp) s 0 of
    Left p -> Left $ show p
    Right (x,_,_) -> case runIntrp (last <$> mapM eval x) of
        Left e -> Left $ show e
        Right a -> Right $ show a
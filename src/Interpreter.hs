module Interpreter(evalExpr) where

import Token(sExp,SExp) 
import Parser(parse,ParseError(..))
import Eval

import Data.Map
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Applicative

newtype Env = Env (Map String SExp) deriving Show
newtype IError = IError String deriving Show

newtype Intrp a = Intrp { run :: ExceptT IError (State (SExp,Env)) a }

evalExpr :: String -> Either [ ParseError Char ] String 
evalExpr s = case parse sExp s 0 of
    Left err -> Left err
    Right (ast,_,_) -> Right $ show $ eval ast 

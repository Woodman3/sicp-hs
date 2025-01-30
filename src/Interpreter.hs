module Interpreter(evalExpr) where

import Token(sExp) 
import Parser(parse,Error(..))
import Eval

evalExpr :: String -> Either [ Error Char ] String 
evalExpr s = case parse sExp s 0 of
    Left err -> Left err
    Right (ast,_,_) -> Right $ show $ eval ast 
module Interpreter(eval_expr) where

import Token(s_exp) 
import Parser(parse,Error(..))
import Eval

eval_expr :: String -> Either [ Error Char ] String 
eval_expr s = case parse s_exp s 0 of
    Left err -> Left err
    Right (ast,_,_) -> Right $ show $ eval ast 
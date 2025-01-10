module Main (main) where

import Token
import Eval
import Parser(run)

import System.IO( openFile, hGetContents, hClose, IOMode(ReadMode) )

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print contents
    case run s_exp contents of
        Left e -> print e
        Right ast -> do
            print "ast"
            print ast
            print "eval"
            print $ eval ast

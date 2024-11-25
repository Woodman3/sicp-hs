module Main (main) where

import Lexer

import System.IO( openFile, hGetContents, hClose, IOMode(ReadMode) )

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print contents
    mapM_ print $ lexer contents
    hClose handle

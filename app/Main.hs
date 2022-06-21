module Main where

import V4.TrivialLakeParser
import V4.Parser.Prim

main = do
    res <- parseProgram "./programs/trivial.js" program
    putStrLn (show res)

module V2.Parser.Prim
( Parser (..)
, item
, notFollowedBy
, eof
, runParser
, parseProgram
, Alternative (..)
) where

import Control.Monad.State
import Control.Applicative

type Input = String

-- Parser is a function that takes a String of characters as input 
-- and yields some tree `a` and the unconsumed part of the input String
-- An empty list denotes failure of a parser, and a singleton list denotes success
type Parser = StateT Input Maybe

runParser :: Parser a -> Input -> Maybe (a, Input)
runParser = runStateT

-- Helper function for parsing a program
parseProgram :: FilePath -> Parser a -> IO (Maybe (a, Input))
parseProgram fp p = do
    file <- readFile fp
    return $ runParser p file

----------------------------------------------------------------------------------
-- Primitive Parsers
----------------------------------------------------------------------------------
-- Succesfully consumes the first character if the input string is non-empty. 
-- Fails otherwise.
item :: Parser Char
item = do
    (x:xs) <- get
    put xs
    return x

-- `notFollowedBy p` only succeeds when parser `p` fails. This parser does not consume any input. 
notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = do
    xs <- get
    case runParser p xs of
        Just _ -> empty
        Nothing -> return ()

eof :: Parser ()
eof = notFollowedBy item

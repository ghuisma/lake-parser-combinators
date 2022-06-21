module V1.Parser.Char
( sat
, char
, digit
, lower
, upper
, letter
, alphanum
) where

import V1.Parser.Prim

-- Takes a predicate and yields a parser that consumes a single character if it
-- satisfies the predicate, and fails otherwise
sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

----------------------------------------------------------------------------------
-- Character Parsers
----------------------------------------------------------------------------------

-- Single character parser
char :: Char -> Parser Char
char x = sat (x ==)

-- Single digit parser
digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

-- Single lowercase letter parser
lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

-- Single uppercase letter parser
upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

-- Single letter parser
letter :: Parser Char
letter = lower <|> upper

-- Single alphanumerical parser
alphanum :: Parser Char
alphanum = letter <|> digit

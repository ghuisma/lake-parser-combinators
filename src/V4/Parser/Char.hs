module V4.Parser.Char
( sat
, char
, digit
, lower
, upper
, letter
, alphanum
) where

import V4.Parser.Prim

-- Takes a predicate and yields a parser that consumes a single character if it
-- satisfies the predicate, and fails otherwise
sat :: (Char -> Bool) -> LakeParser Char
sat p = do
    x <- item
    if p x then return x else empty

----------------------------------------------------------------------------------
-- Character Parsers
----------------------------------------------------------------------------------

-- Single character parser
char :: Char -> LakeParser Char
char x = sat (x ==)

-- Single digit parser
digit :: LakeParser Char
digit = sat (\x -> '0' <= x && x <= '9')

-- Single lowercase letter parser
lower :: LakeParser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

-- Single uppercase letter parser
upper :: LakeParser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

-- Single letter parser
letter :: LakeParser Char
letter = lower <|> upper

-- Single alphanumerical parser
alphanum :: LakeParser Char
alphanum = letter <|> digit

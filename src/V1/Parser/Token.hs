module V1.Parser.Token
( GenLanguageDef (..)
, GenTokenParser (..)
, makeTokenParser
) where

import V1.Parser.Prim
import V1.Parser.Combinator
import V1.Parser.Char

----------------------------------------------------------------------------------
-- Language Definition
----------------------------------------------------------------------------------
data GenLanguageDef
    = LanguageDef
        {
        -- | Describes the start of a block comment
        commentStart   :: String
        ,
        -- | Describes the end of a block comment
        commentEnd     :: String
        ,
        -- | Describes the start of a line comment 
        commentLine    :: String
        ,
        -- | This parser should accept any start characters of identifiers
        identStart     :: Parser Char
        ,
        -- | This parser should accept any legal tail characters of identifiers.
        identLetter    :: Parser Char
        }

----------------------------------------------------------------------------------
-- Token Parser
----------------------------------------------------------------------------------
data GenTokenParser a
    = TokenParser
        {
        -- | This lexeme parser parses a legal identifier. 
        identifier  :: Parser String
        ,
        -- | This lexeme parser parses an integer (a whole number). 
        natural     :: Parser Integer
        ,
        -- | This lexeme parser parses an integer (a whole number). 
        integer     :: Parser Integer
        ,
        -- | Lexeme parser @symbol s@ parses 'string' @s@ and skips trailing white space. 
        symbol      :: String -> Parser String
        ,
        -- | `lexeme p` first applies parser `p`and then the `whiteSpace` parser, returning the value of `p`
        lexeme      :: Parser a -> Parser a
        ,
        -- | Parses any white space. White space consists of /zero/ or more occurrences of a 'space', a line comment or a block (multiline) comment.
        whiteSpace  :: Parser ()
        ,
        -- | Lexeme parser @parens p@ parses @p@ enclosed in parenthesis, returning the value of @p@.
        parens      :: Parser a -> Parser a
        ,
        -- | Lexeme parser @braces p@ parses @p@ enclosed in braces (\'{\' and \'}\'), returning the value of @p@.
        braces      :: Parser a -> Parser a
        ,
        -- | Lexeme parser @brackets p@ parses @p@ enclosed in brackets (\'[\' and \']\'), returning the value of @p@.
        brackets    :: Parser a -> Parser a
        }

----------------------------------------------------------------------------------
-- Given a LanguageDef, create a token parser.
----------------------------------------------------------------------------------
makeTokenParser :: GenLanguageDef -> GenTokenParser a
{-# INLINABLE makeTokenParser #-}
makeTokenParser languageDef
    = TokenParser
        { identifier = identifier
        , natural = lexeme natural
        , integer = lexeme integer
        , symbol = symbol
        , whiteSpace = whiteSpace
        , lexeme = lexeme
        , parens = parens
        , braces = braces
        , brackets = brackets
        }
    where
    -----------------------------------------------------------
    -- Identifiers
    -----------------------------------------------------------
    identifier :: Parser String
    identifier = lexeme $ do
        x <- identStart languageDef
        xs <- many (identLetter languageDef)
        return (x:xs)
    -----------------------------------------------------------
    -- Numbers
    -----------------------------------------------------------
    natural :: Parser Integer
    natural = do
        xs <- some digit
        return (read xs :: Integer)

    integer :: Parser Integer
    integer = op <*> natural
            where op = negate <$ char '-'
                    <|> return id
    -----------------------------------------------------------
    -- Symbols
    -----------------------------------------------------------
    -- Parse a specific string
    string :: String -> Parser String
    string "" = return ""
    string (x:xs) = do
        char x
        string xs
        return (x:xs)
    
    symbol :: String -> Parser String
    symbol = lexeme . string
    -----------------------------------------------------------
    -- Whitespace
    -----------------------------------------------------------
    spaces :: Parser ()
    spaces = do
        _ <- some (sat isSpace)
        return ()
        where isSpace x = (x == ' ') || (x == '\n') || (x == '\t')

    lineComment :: Parser ()
    lineComment = do
        _ <- symbol (commentLine languageDef)
        _ <- many (sat (/= '\n'))
        return ()

    multiLineComment :: Parser ()
    multiLineComment = do
        _ <- symbol $ commentStart languageDef
        _ <- many $ notFollowedBy (symbol (commentEnd languageDef)) *> item
        _ <- symbol $ commentEnd languageDef
        return ()

    whiteSpace :: Parser ()
    whiteSpace = do
        _ <- many (spaces <|> lineComment <|> multiLineComment)
        return ()

    lexeme :: Parser a -> Parser a
    lexeme p = p <* whiteSpace
    -----------------------------------------------------------
    -- Bracketing
    -----------------------------------------------------------
    parens :: Parser a -> Parser a
    parens p = lexeme $ between (symbol "(") (symbol ")") p

    braces :: Parser a -> Parser a
    braces p = lexeme $ between (symbol "{") (symbol "}") p

    brackets :: Parser a -> Parser a
    brackets p = lexeme $ between (symbol "[") (symbol "]") p

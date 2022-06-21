module V3.Parser.Token
( GenLanguageDef (..)
, GenTokenLakeParser (..)
, makeTokenLakeParser
) where

import V3.Parser.Prim
import V3.Parser.Combinator
import V3.Parser.Char
import V3.Parser.Lake
import Control.Monad.Reader

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
        identStart     :: LakeParser Char
        ,
        -- | This parser should accept any legal tail characters of identifiers.
        identLetter    :: LakeParser Char
        }

----------------------------------------------------------------------------------
-- Token Parser
----------------------------------------------------------------------------------
data GenTokenLakeParser a
    = TokenParser
        {
        -- | This lexeme parser parses a legal identifier. 
        identifier  :: LakeParser String
        ,
        -- | This lexeme parser parses an integer (a whole number). 
        natural     :: LakeParser Integer
        ,
        -- | This lexeme parser parses an integer (a whole number). 
        integer     :: LakeParser Integer
        ,
        -- | Lexeme parser @symbol s@ parses 'string' @s@ and skips trailing white space. 
        symbol      :: String -> LakeParser String
        ,
        -- | `lexeme p` first applies parser `p`and then the `whiteSpace` parser, returning the value of `p`
        lexeme      :: LakeParser a -> LakeParser a
        ,
        -- | Parses any white space. White space consists of /zero/ or more occurrences of a 'space', a line comment or a block (multiline) comment.
        whiteSpace  :: LakeParser ()
        ,
        -- | Lexeme parser @parens p@ parses @p@ enclosed in parenthesis, returning the value of @p@.
        parens      :: LakeParser a -> LakeParser a
        ,
        -- | Lexeme parser @braces p@ parses @p@ enclosed in braces (\'{\' and \'}\'), returning the value of @p@.
        braces      :: LakeParser a -> LakeParser a
        ,
        -- | Lexeme parser @brackets p@ parses @p@ enclosed in brackets (\'[\' and \']\'), returning the value of @p@.
        brackets    :: LakeParser a -> LakeParser a
        }

----------------------------------------------------------------------------------
-- Given a LanguageDef, create a token parser.
----------------------------------------------------------------------------------
makeTokenLakeParser :: GenLanguageDef -> GenTokenLakeParser a
{-# INLINABLE makeTokenLakeParser #-}
makeTokenLakeParser languageDef
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
    identifier :: LakeParser String
    identifier = lexeme $ do
        x <- identStart languageDef
        xs <- many (identLetter languageDef)
        return (x:xs)
    -----------------------------------------------------------
    -- Numbers
    -----------------------------------------------------------
    natural :: LakeParser Integer
    natural = do
        xs <- some digit
        return (read xs :: Integer)

    integer :: LakeParser Integer
    integer = op <*> natural
            where op = negate <$ char '-'
                    <|> return id
    -----------------------------------------------------------
    -- Symbols
    -----------------------------------------------------------
    -- Parse a specific string
    string :: String -> LakeParser String
    string "" = return ""
    string (x:xs) = do
        char x
        string xs
        return (x:xs)
    
    symbol :: String -> LakeParser String
    symbol = lexeme . string
    -----------------------------------------------------------
    -- Whitespace
    -----------------------------------------------------------
    spaces :: LakeParser ()
    spaces = do
        _ <- some (sat isSpace)
        return ()
        where isSpace x = (x == ' ') || (x == '\n') || (x == '\t')

    lineComment :: LakeParser ()
    lineComment = do
        _ <- symbol (commentLine languageDef)
        _ <- many (sat (/= '\n'))
        return ()

    multiLineComment :: LakeParser ()
    multiLineComment = do
        _ <- symbol $ commentStart languageDef
        _ <- many $ notFollowedBy (symbol (commentEnd languageDef)) *> item
        _ <- symbol $ commentEnd languageDef
        return ()

    whiteSpace :: LakeParser ()
    whiteSpace = do
        _ <- many (spaces <|> lineComment <|> multiLineComment)
        return ()

    lexeme :: LakeParser a -> LakeParser a
    lexeme p = p <* whiteSpace
    -----------------------------------------------------------
    -- Bracketing
    -----------------------------------------------------------
    parens :: LakeParser a -> LakeParser a
    parens p = lexeme $ between (symbol "(") (symbol ")") p

    braces :: LakeParser a -> LakeParser a
    braces p = lexeme $ between (symbol "{") (symbol "}") p

    brackets :: LakeParser a -> LakeParser a
    brackets p = lexeme $ between (symbol "[") (symbol "]") p

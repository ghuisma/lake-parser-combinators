module V1.JavaScriptLexer 
( identifier
, integer
, natural
, symbol
, whiteSpace
, parens
, braces
, brackets
) where

import qualified V1.Parser.Token as Token
import V1.Parser.Prim
import V1.Parser.Char

-- Parsec JavaScript language definition, used to create lexer. 
languageDef
    = Token.LanguageDef 
        { 
        -- block comments start with /*
        Token.commentStart    = "/*"
        ,
        -- block comments end with */
        Token.commentEnd      = "*/"     
        , 
        -- line comments start with //
        Token.commentLine     = "//"     
        , 
        -- identifiers start with a letter, an underscore or a dollar sign
        Token.identStart      = letter <|> char '_' <|> char '$'     
        , 
        -- the zero or more characters following an identifier start are alphanumerical, underscores or dollar signs.
        Token.identLetter     = alphanum <|> char '_' <|> char '$'   
        }

-- Create lexer (=tokenizer) for our language.
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

integer :: Parser Integer
integer = Token.integer lexer

natural :: Parser Integer
natural = Token.natural lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

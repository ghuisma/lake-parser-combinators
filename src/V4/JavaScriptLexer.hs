module V4.JavaScriptLexer 
( identifier
, integer
, natural
, symbol
, whiteSpace
, parens
, braces
, brackets
) where

import qualified V4.Parser.Token as Token
import V4.Parser.Prim
import V4.Parser.Char

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
lexer = Token.makeTokenLakeParser languageDef

identifier :: LakeParser String
identifier = Token.identifier lexer

integer :: LakeParser Integer
integer = Token.integer lexer

natural :: LakeParser Integer
natural = Token.natural lexer

symbol :: String -> LakeParser String
symbol = Token.symbol lexer

whiteSpace :: LakeParser ()
whiteSpace = Token.whiteSpace lexer

lexeme :: LakeParser a -> LakeParser a
lexeme = Token.lexeme lexer

parens :: LakeParser a -> LakeParser a
parens = Token.parens lexer

braces :: LakeParser a -> LakeParser a
braces = Token.braces lexer

brackets :: LakeParser a -> LakeParser a
brackets = Token.brackets lexer

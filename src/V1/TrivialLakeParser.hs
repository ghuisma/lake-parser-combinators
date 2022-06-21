module V1.TrivialLakeParser ( program ) where

import V1.JavaScriptLexer
import V1.Parser.Prim

----------------------------------------------------------------------------------------
-- Island/Lake Parsing Section
----------------------------------------------------------------------------------------

data IslandOrWater a = Island a
                     | Water
                     deriving (Show, Eq)

----------------------------------------------------------------------------------------
-- Trivial Parser Implementation
----------------------------------------------------------------------------------------

type Identifier = [Char]

data Stmt = FunDef Identifier (IslandOrWater Block)
          | FunCall Identifier
          deriving (Show, Eq)

newtype Block = Block [IslandOrWater Stmt]
              deriving (Show, Eq)

program :: Parser [IslandOrWater Stmt]
program = do
    whiteSpace
    program <- many programSea
    eof
    return program
    
programSea :: Parser (IslandOrWater Stmt)
programSea = funDef <|> funCall <|> programWater

programWater :: Parser (IslandOrWater Stmt)
programWater = do
    item
    return Water

funDef :: Parser (IslandOrWater Stmt)
funDef = do
    symbol "function"
    id <- identifier
    symbol "("
    param
    symbol ")"
    bl <- block
    return $ Island $ FunDef id bl

funCall :: Parser (IslandOrWater Stmt)
funCall = do
    id <- identifier
    symbol "("
    param
    symbol ")"
    return $ Island $ FunCall id

param :: Parser [IslandOrWater Stmt]
param = many paramSea

paramSea :: Parser (IslandOrWater Stmt)
paramSea = paramWater

paramWater :: Parser (IslandOrWater Stmt)
paramWater = do
    notFollowedBy (symbol ")")
    item
    return Water

block :: Parser (IslandOrWater Block)
block = do
    symbol "{"
    stmts <- many stmt
    symbol "}"
    return $ Island $ Block stmts

stmt :: Parser (IslandOrWater Stmt)
stmt = stmtSea

stmtSea :: Parser (IslandOrWater Stmt)
stmtSea = funDef <|> funCall <|> stmtWater

stmtWater :: Parser (IslandOrWater Stmt)
stmtWater = do
    notFollowedBy (symbol "}")
    item
    return Water

module V3.TrivialLakeParser ( program ) where

import V3.Parser.Prim
import V3.Parser.Lake
import V3.JavaScriptLexer

----------------------------------------------------------------------------------------
-- Trivial Lake Parser Implementation
----------------------------------------------------------------------------------------

type Identifier = [Char]

data Stmt = FunDef Identifier (IslandOrWater Block)
          | FunCall Identifier
          deriving (Show, Eq)

newtype Block = Block [Stmt]
              deriving (Show, Eq)

program :: LakeParser [Stmt]
program = do
    whiteSpace
    program <- stmtLake
    eof
    return program

funDef :: LakeParser (IslandOrWater Stmt)
funDef = island ')' $ do
    symbol "function"
    id <- identifier
    symbol "("
    emptyLake
    symbol ")"
    bl <- block
    return $ FunDef id bl

funCall :: LakeParser (IslandOrWater Stmt)
funCall = island ')' $ do
    id <- identifier
    symbol "("
    emptyLake
    symbol ")"
    return $ FunCall id

block :: LakeParser (IslandOrWater Block)
block = island '}' $ do
    symbol "{"
    stmts <- stmtLake
    symbol "}"
    return $ Block stmts

stmtLake :: LakeParser [Stmt]
stmtLake = lake (funDef <|> funCall)

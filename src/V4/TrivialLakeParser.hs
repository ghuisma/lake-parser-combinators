-- module V4.TrivialLakeParser ( program ) where

import V4.Parser.Prim
import V4.Parser.Lake
import V4.JavaScriptLexer
import Control.Monad.Reader

----------------------------------------------------------------------------------------
-- Trivial Lake Parser Implementation
----------------------------------------------------------------------------------------
type Identifier = [Char]

data Stmt = FunDef Identifier (IslandOrWater Block)
          | FunCall Identifier
          deriving (Show, Eq)

newtype Block = Block [IslandOrWater Stmt]
              deriving (Show, Eq)

program :: LakeParser [IslandOrWater Stmt]
program = do
    whiteSpace
    program <- stmtLake
    eof
    return program

funDef :: LakeParser (IslandOrWater Stmt)
funDef = island $ do
    symbol "function"
    id <- identifier
    symbol "("
    emptyLake
    symbol ")"
    bl <- block
    return $ FunDef id bl

funCall :: LakeParser (IslandOrWater Stmt)
funCall = island $ do
    id <- identifier
    symbol "("
    emptyLake
    symbol ")"
    return $ FunCall id

block :: LakeParser (IslandOrWater Block)
block = island $ do
    symbol "{"
    stmts <- stmtLake
    symbol "}"
    return $ Block stmts

stmtLake :: LakeParser [IslandOrWater Stmt]
stmtLake = lake (funDef <|> funCall)

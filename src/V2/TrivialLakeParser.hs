module V2.TrivialLakeParser ( program ) where

import V2.Parser.Prim
import V2.Parser.Char
import V2.Parser.Lake
import V2.JavaScriptLexer
import qualified Data.Set as Set

----------------------------------------------------------------------------------------
-- Trivial Lake Parser Implementation
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
    program <- stmtLake Set.empty
    eof
    return program

funDef :: LakeParser (IslandOrWater Stmt)
funDef = island ')' $ \s -> do
    symbol "function"
    id <- identifier
    symbol "("
    emptyLake s
    symbol ")"
    bl <- block s
    return $ FunDef id bl

funCall :: LakeParser (IslandOrWater Stmt)
funCall = island ')' $ \s -> do
    id <- identifier
    symbol "("
    emptyLake s
    symbol ")"
    return $ FunCall id

block :: LakeParser (IslandOrWater Block)
block = island '}' $ \s -> do
    symbol "{"
    stmts <- stmtLake s
    symbol "}"
    return $ Block stmts

stmtLake :: LakeParser [IslandOrWater Stmt]
stmtLake = lake [funDef, funCall]

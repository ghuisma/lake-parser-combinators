module V2.Parser.Lake 
( IslandOrWater (..)
, LakeParser
, emptyLake
, lake
, water
, island
) where

import V2.Parser.Prim
import V2.Parser.Char
import qualified Data.Set as Set

data IslandOrWater a = Island a
                     | Water
                     deriving (Show, Eq)

type Alt = Char   -- alternative symbols

type AltSet = Set.Set Alt  -- set of current alternative symbols

type LakeParser a = AltSet -> Parser a

-- Receives the alternative symbols map
-- Returns the parser for a lake without any islands in it.
emptyLake :: LakeParser [IslandOrWater a]
emptyLake = lake []

-- Receives the parsers for the islands inside the lake and the alternative symbols map.
-- Returns combined island and water parsers
lake :: [LakeParser (IslandOrWater a)] -> LakeParser [IslandOrWater a]
lake ps s = many $ foldl (\acc p -> acc <|> p s) empty ps <|> water s

-- Receives array of alternative symbols.
-- Parses any token if the lookahead does not match an alternative symbol, i.e. when `notFollowedBy` succeeds for all alternative symbols.
-- Always succeeds if there are not alternative symbols in the AltMap
water :: LakeParser (IslandOrWater a)
water s = do
    foldl (\acc alt -> acc *> (notFollowedBy . char) alt) (return ()) s
    item
    return Water

-- island adds alternative symbol to AltMap, executes parser and removes it at the end
island :: Alt -> (LakeParser a) -> LakeParser (IslandOrWater a)
island a p s = Island <$> p (Set.insert a s)

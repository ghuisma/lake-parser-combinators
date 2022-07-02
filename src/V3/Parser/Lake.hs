module V3.Parser.Lake 
( IslandOrWater (..)
, emptyLake
, lake
, water
, island
) where

import Control.Monad.Reader
import V3.Parser.Prim
import V3.Parser.Char
import qualified Data.Set as Set

data IslandOrWater a = Island a
                     | Water
                     deriving (Show, Eq)

-- Receives the alternative symbols map
-- Returns the parser for a lake without any islands in it.
emptyLake :: LakeParser [IslandOrWater a]
emptyLake = lake empty

-- Receives the parsers for the islands inside the lake and the alternative symbols map.
-- Returns combined island and water parsers
lake :: LakeParser (IslandOrWater a) -> LakeParser [IslandOrWater a]
lake lp = many $ lp <|> water

-- Receives array of alternative symbols.
-- Parses any token if the lookahead does not match an alternative symbol, i.e. when `notFollowedBy` succeeds for all alternative symbols.
-- Always succeeds if there are not alternative symbols in the AltMap
water :: LakeParser (IslandOrWater a)
water = do
    s <- ask
    foldl (\acc alt -> acc *> (notFollowedBy . char) alt) (return ()) s
    item
    return Water

-- island adds alternative symbol to AltMap, executes parser and removes it at the end
island :: Alt -> (LakeParser a) -> LakeParser (IslandOrWater a)
island a = (Island <$>) . local (Set.insert a)

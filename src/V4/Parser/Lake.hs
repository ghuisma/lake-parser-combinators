{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module V4.Parser.Lake 
( IslandOrWater (..)
, emptyLake
, lake
, island
) where

import V4.Parser.Prim
import V4.Parser.Char
import Data.List (tails, inits)
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
lake lp = do
    accAltFlag <- askAccAltFlag
    if accAltFlag
        then return [] -- skip
        else many $ lp <|> water

-- Receives array of alternative symbols.
-- Parses any token if the lookahead does not match an alternative symbol, i.e. when `notFollowedBy` succeeds for all alternative symbols.
-- Always succeeds if there are not alternative symbols in the AltMap
water :: LakeParser (IslandOrWater a)
water = do
    altSet <- askAltSet
    foldl (\acc (Alt alt) -> acc *> (notFollowedBy . char) alt) (return ()) altSet
    item
    return $ Water

-- island adds alternative symbol to AltMap, executes parser and removes it at the end
island :: LakeParser a -> LakeParser (IslandOrWater a)
island lp = Island <$> do
    accAltFlag <- askAccAltFlag
    if accAltFlag
        then empty -- stop accumulation
        else do
            xs <- get
            case accAlt lp xs of
                Nothing -> lp
                Just alt -> localAltSet (Set.insert alt) lp

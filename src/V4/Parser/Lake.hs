{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module V4.Parser.Lake 
( IslandOrWater (..)
, emptyLake
, lake
, water
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
emptyLake = lake []

-- Receives the parsers for the islands inside the lake and the alternative symbols map.
-- Returns combined island and water parsers
lake :: [LakeParser (IslandOrWater a)] -> LakeParser [IslandOrWater a]
lake rs = do
    many $ foldl (<|>) empty rs <|> water

-- Receives array of alternative symbols.
-- Parses any token if the lookahead does not match an alternative symbol, i.e. when `notFollowedBy` succeeds for all alternative symbols.
-- Always succeeds if there are not alternative symbols in the AltMap
water :: LakeParser (IslandOrWater a)
water = do
    altSet <- ask
    foldl (\acc (Alt alt) -> acc *> (notFollowedBy . char) alt) (return ()) altSet
    item
    return $ Water

-- island adds alternative symbol to AltMap, executes parser and removes it at the end
island :: LakeParser a -> LakeParser (IslandOrWater a)
island lp = Island <$> do
    xs <- get
    case snd $ runLakeParser lp xs of
        Nothing -> lp
        Just alt -> local (Set.insert alt) lp

------------------------------------------------------------------------
-- Attempt at providing correct input to runLakeParser
------------------------------------------------------------------------
-- island :: LakeParser a -> LakeParser (IslandOrWater a)
-- island lp = Island <$> do
--     xs <- get 
--     island' lp (combs xs)

-- island' :: LakeParser a -> [String] -> LakeParser a
-- island' lp [] = empty
-- island' lp (xs:xss) = case snd $ runLakeParser lp xs of
--     Nothing -> island' lp xss
--     Just alt -> local (Set.insert alt) lp

-- combs :: String -> [String]
-- combs = ([] :) . concatMap (tail . inits) . tails

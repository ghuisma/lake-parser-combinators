# Changelog

## V1
- Implemented basic monadic parser combinator library
- Created `TrivialLakeParser.hs`, a working lake parser for `/programs/trivial.js`

## V2
- Implemented basic lake combinators
- Changed `TrivialLakeParser.hs` to use newly defined set of lake combinators

## V3
- Created `LakeParser`, which is the old `Parser` wrapped in `ReaderT AltSet`, allowing us to automagically pass the `AltSet` to lake combinators. Thereby hiding this implementation detail from the end-user.

## V4 (WORK IN PROGRESS)
- Added `Writer (Maybe Alt)` to the bottom of the `LakeParser` monad stack, replacing `Maybe` with `MaybeT (Writer Maybe Alt)` 
- the primitive `item` parser combinator now also `tell`s the item we parsed, allowing us to access the last parsed item of a parse (= alternative symbol of that specific parser)

## V5 (TODO)
- Added QuickCheck tests for validating the recusive island parsing functionalities.

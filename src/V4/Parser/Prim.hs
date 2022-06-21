{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module V4.Parser.Prim
( LakeParser (..)
, Input
, AltSet (..)
, Alt (..)
, ask
, local
, get
, item
, notFollowedBy
, eof
, runLakeParser
, parseProgram
, Alternative (..)
) where

import Control.Applicative
import Control.Monad.Fail
import Control.Monad.State.Lazy as State
import Control.Monad.Reader as Reader
import Control.Monad.Writer.Lazy as Writer (MonadWriter (..), Writer, runWriter, execWriter, tell)
import Control.Monad.Trans.Maybe as Maybe
import qualified Data.Set as Set

type Input = String

type AltSet = Set.Set Alt  -- set of current alternative symbols

newtype Alt = Alt Char   -- alternative symbols
            deriving (Show, Ord, Eq)

-- implement the associative function (<>) of Semigroup,
-- such that when we start combining Alt's, the result will
-- be the final Alt combined.
instance Semigroup Alt where
    _ <> y = y

newtype LakeParser a = LakeParser { getLakeParser :: ReaderT AltSet (StateT Input (MaybeT (Writer (Maybe Alt)))) a
                                  } deriving ( Monad
                                             , Applicative
                                             , Functor
                                             , Alternative
                                             , MonadFail
                                             , MonadState Input
                                             , MonadWriter (Maybe Alt)
                                             , MonadReader AltSet
                                             )

runLakeParser :: LakeParser a -> Input -> (Maybe (a, Input), Maybe Alt)
runLakeParser lp input = runWriter $ runMaybeT $ runStateT (runReaderT (getLakeParser lp) Set.empty) input

-- Helper function for parsing a program
parseProgram :: FilePath -> LakeParser a -> IO (Maybe (a, Input))
parseProgram fp p = do
    file <- readFile fp
    return $ fst $ runLakeParser p file

----------------------------------------------------------------------------------
-- Primitive Parsers
----------------------------------------------------------------------------------
-- Succesfully consumes the first character if the input string is non-empty. 
-- Fails otherwise.
item :: LakeParser Char
item = do
    (x:xs) <- get
    tell (Just (Alt x))
    put xs
    return x

-- `notFollowedBy p` only succeeds when parser `p` fails. This parser does not consume any input. 
notFollowedBy :: LakeParser a -> LakeParser ()
notFollowedBy p = do
    xs <- get
    case fst $ runLakeParser p xs of
        Just _ -> empty
        Nothing -> return ()

eof :: LakeParser ()
eof = notFollowedBy item

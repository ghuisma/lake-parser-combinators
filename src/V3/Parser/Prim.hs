{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module V3.Parser.Prim
( LakeParser (..)
, Alt
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
import qualified Data.Set as Set

type Input = String

type AltSet = Set.Set Alt  -- set of current alternative symbols

type Alt = Char   -- alternative symbols

newtype LakeParser a = LakeParser { getLakeParser :: ReaderT AltSet (StateT Input Maybe) a
                                  } deriving ( Monad
                                             , Applicative
                                             , Functor
                                             , Alternative
                                             , MonadFail
                                             , MonadState Input
                                             , MonadReader AltSet
                                             )

runLakeParser :: LakeParser a -> String -> Maybe (a, String)
runLakeParser lp = runStateT (runReaderT (getLakeParser lp) Set.empty)

-- Helper function for parsing a program
parseProgram :: FilePath -> LakeParser a -> IO (Maybe (a, String))
parseProgram fp p = do
    file <- readFile fp
    return $ runLakeParser p file

----------------------------------------------------------------------------------
-- Primitive Parsers
----------------------------------------------------------------------------------
-- Succesfully consumes the first character if the input string is non-empty. 
-- Fails otherwise.
item :: LakeParser Char
item = do
    (x:xs) <- get
    put xs
    return x

-- `notFollowedBy p` only succeeds when parser `p` fails. This parser does not consume any input. 
notFollowedBy :: LakeParser a -> LakeParser ()
notFollowedBy p = do
    xs <- get
    case runLakeParser p xs of
        Just _ -> empty
        Nothing -> return ()

eof :: LakeParser ()
eof = notFollowedBy item

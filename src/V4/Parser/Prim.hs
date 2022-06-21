{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module V4.Parser.Prim
( LakeParser (..)
, Input
, Env (..)
, AltSet (..)
, Alt (..)
, askAltSet
, askAccAltFlag
, localAltSet
, localAccAltFlag
, get
, tellAlt
, item
, notFollowedBy
, eof
, runLakeParser
, accAlt
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

data Env = Env { altSet :: AltSet
               , accAltFlag :: Bool
               } deriving (Show)

tellAlt :: Char -> LakeParser ()
tellAlt = tell . Just . Alt

askAltSet :: LakeParser AltSet
askAltSet = asks altSet

askAccAltFlag :: LakeParser Bool
askAccAltFlag = asks accAltFlag

localAltSet :: (AltSet -> AltSet) -> LakeParser a -> LakeParser a
localAltSet f = local (\env@(Env{altSet=s}) -> env { altSet = (f s)})

localAccAltFlag :: Bool -> LakeParser a -> LakeParser a
localAccAltFlag x = local (\env -> env { accAltFlag = x})

newtype Alt = Alt Char   -- alternative symbols
            deriving (Show, Ord, Eq)

-- implement the associative function (<>) of Semigroup,
-- such that when we start combining Alt's, the result will
-- be the final Alt combined.
instance Semigroup Alt where
    _ <> y = y

newtype LakeParser a = LakeParser { getLakeParser :: ReaderT Env (StateT Input (MaybeT (Writer (Maybe Alt)))) a
                                  } deriving ( Monad
                                             , Applicative
                                             , Functor
                                             , Alternative
                                             , MonadFail
                                             , MonadState Input
                                             , MonadWriter (Maybe Alt)
                                             , MonadReader Env
                                             )

runLakeParser :: LakeParser a -> Input -> Maybe (a, Input)
runLakeParser lp input = fst $ runWriter $ runMaybeT $ runStateT (runReaderT (getLakeParser lp) (Env Set.empty False)) input

accAlt :: LakeParser a -> Input -> (Maybe Alt)
accAlt lp input = snd $ runWriter $ runMaybeT $ runStateT (runReaderT (getLakeParser lp) (Env Set.empty True)) input

-- Helper function for parsing a program
parseProgram :: FilePath -> LakeParser a -> IO (Maybe (a, Input))
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

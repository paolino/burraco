{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module Burraco where

--import Control.Arrow (first, second, (&&&))
--import Data.List (delete, group, sort)
--import Data.Function (on)
import Control.Monad (liftM2)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader, ask , local)
import Test.QuickCheck
--import Data.Either (lefts)
--import Data.Char (chr)
import Control.Parallel.Strategies (NFData)
import Card
import Game
--import Debug.Trace


	

data Contesto = Contesto {
	dotazione :: Cards,
	dotazioneAvversario :: (Cards,Int),
	banco :: [Cards],
	bancoAvversario :: [Cards],
	monteScarti :: Cards,
	pozzetto :: Bool,
	pozzettoAvversario :: Bool,
	tallone :: Int
	}

newtype Contestuale m a = Contestuale (ReaderT Contesto m a)  deriving (Monad, MonadReader Contesto,Functor)

newtype Value = Value Double deriving (Eq,Show,Num,NFData)
mkValue = Value . press
	where press x = (pi/2 + atan x)/pi

type Feature = [(Cards, Value)]



instance Arbitrary Contesto where

	






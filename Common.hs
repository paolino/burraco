{-# LANGUAGE NoMonomorphismRestriction #-}
module Common
  where

import Control.Arrow (first)
import Data.List (delete, groupBy, sortBy)
import Data.Function (on)
import Control.Monad

type Rank = Int
type Suite = Int

type Card = (Rank, Suite)
type Hand = [Card]

data PSA = Piazzato Rank Bool  | Spiazzato Bool | Assente deriving (Show,Eq,Ord)
data Game 
	= Tris {
		grank :: Rank,
		gjolly :: Bool
		}
	| Scala {
		gsuite :: Suite,
		gmatta :: PSA,
		gbirank :: (Rank,Rank)
		}
	| Tavolo
	| Scarto [Card] deriving (Eq,Ord,Show)

type Board = [Either Game Game]
rank = fst
suite = snd
jolly = (-20,-20)

isJP = liftM2 (||) (ranked 2) (ranked (-20))


ranked n = (==) n . fst
suited s = (==) s . snd


nubOrdBy f = map head . groupBy ((==) `on` f) . sortBy (compare `on` f)
nubOrd = nubOrdBy id

x &.& y = liftM2 (&&) x y


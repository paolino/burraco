
{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module Card where

import Control.Arrow (first, second, (&&&))
import Data.List (delete, group, sort)
import Data.Function (on)
import Control.Monad (liftM2, replicateM)
import Data.Either (lefts)
import Data.Char (chr)
import Test.QuickCheck (Arbitrary(..), elements)
import Control.Parallel.Strategies (NFData, rnf)
import Control.Monad.State (lift, evalStateT, StateT, put, get)
import System.Random (randomIO)


-- | Valore di una carta di un qualunque seme. 
newtype Rank = Rank Int deriving (Read,Ord,Eq,Num, Enum,NFData,Integral,Real,Arbitrary)

instance Show Rank where
	show (Rank x) 
		| x == 1 = "A"
		| x == 13 = "K"
		| x == 12 = "Q"
		| x == 11 = "J"
		| x == 14 = "A"
		| otherwise = show $ x
-- | Seme di una carta.
newtype Suite = Suite Int deriving (Read,Ord,Eq,Num,NFData, Enum,Arbitrary)

instance Show Suite where
	show (Suite y) 
		| y == 0 = "C"
		| y == 1 = "Q"
		| y == 2 = "F"
		| y == 3 = "P"

-- | Una carta contiene il suo valore ed il suo seme.I  jolly sono definiti con valore -20 sia per il seme che per il valore
newtype Card = Card {unCard :: (Rank, Suite)} deriving (Read,Eq, Ord,NFData)

instance Arbitrary Card where
	arbitrary = elements deck
		


instance Show Card where
	show (Card (x,y)) 
		| x == -20 = "Jolly"
		| otherwise = show x ++ show y


mkCard :: Rank -> Suite -> Card
mkCard r s = Card (((r - 1) `mod` 13) + 1, s) 
-- | una carta che rappresenta il jolly
jolly :: Card
jolly = Card (-20,-20)

-- | controlla che il valore di una carta sia pari al dato
ranked :: Rank -> Card -> Bool
ranked n = (==) n . fst . unCard

-- | controlla che il seme di una carta sia quello dato
suited :: Suite -> Card -> Bool
suited s = (==) s . snd . unCard

-- | equality compares suites of two cards 
suitedlike :: Card -> Card -> Bool
suitedlike (Card (_,s1)) (Card (_,s2)) = s1 == s2

-- | controlla se una carta e' un jolly o una pinella
isJP :: Card -> Bool
isJP = liftM2 (||) (ranked 2) (ranked (-20))

-- | list of cards in a 2-deck
deck :: [Card]
deck = concat . replicate 2 $ [Card (i,Suite j) | i <- [1..13] , j <- [0..3]] ++ replicate 2 jolly

-- | list of valid cards
cards :: [Card]
cards = map head . group . sort $ deck

------------------- Random Picking in IO ----------------------------------
type Pick = StateT [Card] IO 

-- | pick a new card from the left ones
pickT :: Pick Card
pickT = do
	rs <- get
	n <- (`mod` length rs) `fmap` lift randomIO
	put $ take n rs ++ drop (n + 1) rs
	return $ rs !! n

-- | pick a bunch of cards from the left ones
handT :: Int -> Pick [Card]
handT n  = replicateM n pickT

-- | execute a picking session on a new deck
runT :: Pick a -> IO a
runT f = evalStateT f deck



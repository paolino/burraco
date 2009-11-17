{-# LANGUAGE GeneralizedNewtypeDeriving,TypeSynonymInstances, DeriveFunctor #-}


module Game (
	giochi,
	Cards ,
	)
	where

import Card
import Test.QuickCheck 
import Control.Parallel.Strategies (NFData)
import Data.List (sort, group)
import Control.Concurrent
import Bag 
import qualified Data.Map

--------- Powerset library ------------------------

-- | compute the subsets of a set, including the empty one (run in exponential space, care)
powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = xss ++ map (x:) xss
	    where xss = powerset xs

-------------- Definizione dei giochi ------------------
-- | specific Bag of cards
type Cards = Bag Card
-- | chiamiamo gioco un insieme di carte che rappresenta un gioco teorico come "la scala di cuori dal 4 al 9" etc

-- | tutte le scale possibili 
scale :: [Cards]
scale = map fromList $ do
	j <- [0 .. 3]
	n <- [0 .. 13] 
	x <- [1 .. 14 - n]
	return . map (flip mkCard j) $ [x .. x + n] 
		
-- | tutti i tris possibili
trisses :: [Cards]
trisses = let 	f j = concatMap $ \x -> [x] ++  if j `elem` x then [j:x] else []
	in map fromList $ do
		n <- [1] ++ [3 .. 13]
		x <- foldr f (tail . powerset $ [0 .. 3]) [0..3]
		return . map (mkCard n) $ x

-- | tutti i giochi possibili
giochi :: [Cards]
giochi = map head. group . sort $ scale ++ trisses  

------------------  Test --------------------
instance Arbitrary Gioco where
	arbitrary = do
		t <- elements [False,True]
		case t of 
			True -> do 
				c1 <- elements [1..14]
				c2 <- elements [c1.. 14]
				s <- elements [0..3] 
				return . Scala . map (flip mkCard s) $ [c1 .. c2] 
			False -> do
				s <- elements $ tail $ powerset [0,0,1,1,2,2,3,3]
				r <- elements $ [1] ++ [3..13]
				return . Tris . map (mkCard r) $ s

data Gioco = Scala {unGioco :: [Card]} | Tris {unGioco :: [Card]} deriving Show

main = do 	print "Testing Giochi ..............."
		quickCheckWith stdArgs {maxSuccess = 10000} (\cs -> fromList (unGioco cs) `elem` giochi) 

{-# LANGUAGE GeneralizedNewtypeDeriving,TypeSynonymInstances, FlexibleInstances, DeriveFunctor #-}


module Game (
	giochi,
	Cards ,
	powerset, 
	giocoGen
	)
	where

import Card
import Test.QuickCheck 
import Control.Parallel.Strategies (NFData)
import Data.List (sort, group, delete)
import Control.Concurrent
import Control.Monad.State
import qualified Data.Set as S (Set,toList, fromList, member, union)
import qualified Enumeration as E
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

-- | i giochi in un Set
setGiochi :: S.Set Cards
setGiochi = S.fromList $ giochi

-- | set dei giochi, compresi quelli bucati
setHoleds :: S.Set Cards
setHoleds = S.union setGiochi . S.fromList $ do
	g <- map toList giochi
	h <- g
	return $ fromList (delete h g) 

-- | estrae le matte dal mazzo
jollies :: Cards -> Cards
jollies = fromList . filter isJP . toList 



deckBag = fromList deck


giocoGen :: Gen Cards
giocoGen = do
	g <- elements . S.toList $ setGiochi 
	j <- elements $ [False,True]
	if j then do
		let cs = toList g
		c <- elements cs
		j <- elements $ filter isJP $ deck
		return $ insertion j (fromList $ delete c cs)
		else return g
	

 
main = do 	print "Testing Giochi ..............."
		
		

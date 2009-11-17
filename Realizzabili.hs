
{-# LANGUAGE GeneralizedNewtypeDeriving,DeriveFunctor #-}
module Realizzabili (realizzabili) 
	where

import qualified Data.Map as M
import Control.Arrow ((&&&), (***))
import Bag (difference ,size, unions , intersection, toList, empty, union, fromList)
import Game (Cards, giochi)
import Burraco (dotazione, Feature, Contestuale , mkValue, Value , Contesto (..))
import Card (Card, cards)
import Control.Monad.Reader (asks, ask)
import Data.Maybe (isJust, fromJust)

import Test.QuickCheck
import Control.Parallel.Strategies
import Control.Parallel


-- | la probabilita' di ottenere una carta
chance012 :: Monad m => Contestuale m (M.Map Card Int)
chance012 = do 
	Contesto _ _ b ba _ _ _ _ <- ask
	let 	f c = 2 - (size . intersection (fromList $ replicate 2 c) $ unions (b ++ ba))		
	return $ M.fromList (map (id &&& f) cards) 	
	

rchance :: Monad m => [Cards] -> Contestuale m [Maybe Value]
rchance xs = do
	cs <- chance012
	return $  do 
		ys <- map toList xs 
		let 	zs = map (fromIntegral . (cs M.!)) ys
		 	zeri = filter (== 0) zs
		if length zeri > 1 then return Nothing
			else return . Just . product $ map mkValue zs
		
-- | tutti i giochi che intersecano la dotazione con un valore di probabilità di realizzazione. I giochi che hanno 2 o piu' carte irrecuperabili non appaiono.
realizzabili :: Monad m => Contestuale m Feature
realizzabili = do
	d <- asks dotazione
	let rs = filter (not . empty . fst) . map (id &&& flip difference d) $ giochi
	(map (id *** fromJust) . filter (isJust . snd) . zipWith ((,) . fst) rs) `fmap` rchance (map snd rs)

main = do
	print "testing Realizzabili...."

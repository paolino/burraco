{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Function (on)
import System.Random
import Data.List (sortBy)
import Control.Monad.State
import Control.Arrow (second, (***))
import System.Environment
import Enumeration
import Burraco


----------------- tests -------------------------------

deck :: [Card]
deck = concat . replicate 2 $ [Card (i,Suite j) | i <- [1..13] , j <- [0..3]] ++ replicate 4 jolly

pickT = do
	rs <- get
	n <- (`mod` length rs) `fmap` lift randomIO
	put $ take n rs ++ drop (n + 1) rs
	return $ rs !! n

handT n  = replicateM n pickT

runT f = evalStateT f deck
main = do
	r:_ <- getArgs
	h0 <- runT $ handT (read r) 
	print h0
	mapM_ print . map (second $ map env) $ solutions (h0,[GState [] Tavolo, GState [] Scarto ])
{-
main' = do
	h:_ <- getArgs
	let 	h0 = map (Card . (Rank *** Suite)) $ read h :: Hand
	mapM_ print . map (second $ map game) . snd . run (sviluppo) $ (h0,[GState [] Tavolo, GState [] Scarto ])
-}

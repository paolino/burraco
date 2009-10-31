{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Function (on)
import System.Random
import Data.List (sortBy)
import Control.Monad.State
import Enumeration
import Common
import System.Environment


----------------- tests -------------------------------

deck :: [Card]
deck = concat . replicate 2 $ [(i,j) | i <- [1..13] , j <- [0..3]] ++ replicate 4 jolly

pickT = do
	rs <- get
	n <- (`mod` length rs) `fmap` lift randomIO
	put $ take n rs ++ drop (n + 1) rs
	return $ rs !! n

handT n  = replicateM n pickT

runT f = evalStateT f deck
	
main = do
	r:_ <- getArgs
	h <- runT $ handT (read r)
	mapM_ print $ sortBy (compare `on` (length . fst)) $ nubOrd $ sviluppo0 h [Tavolo, Scarto (Right [])]

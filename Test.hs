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

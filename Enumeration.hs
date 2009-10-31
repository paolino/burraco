module Enumeration 
	where

import Data.List (delete,(\\))
import Data.Either
import Common
import Games

type Move = (Hand,Board)


sviluppo0 :: Hand -> Board -> [Move]
sviluppo0 h b = do 
	c <- h
	g <- rights b
	r <- attach g c (delete c h)
	let 	v = case r of 
			R (g',h') -> (h', Right g' : delete (Right g) b)
			N (g',h') -> (h', Right g' : b)
			E (g',h') -> (h', Left g' : delete (Right g) b)
		rs = uncurry sviluppo0 v
	case rs of
		[] -> return v
		some -> some



module Enumeration 
	where

import Data.List (delete)

import Common
import Games


sviluppo0 :: Hand -> Board -> [(Hand,Board)]
sviluppo0 h b = do 
	c <- h
	g <- b
	(g',h') <- attach g c (delete c h)
	let 	b' = g' : case g of Tavolo -> b ; _ -> delete g b
		rs = sviluppo0 h' b'
	case rs of
		[] -> return (h', b')
		some -> some


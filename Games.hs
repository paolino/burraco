{-# LANGUAGE NoMonomorphismRestriction #-}
module Games where

import Control.Arrow (first)
import Data.List (delete, groupBy, sortBy)
import Data.Function (on)
import Control.Monad (liftM2)
import Common

type Index = Int
data Jolly a = None | Given a | Internal (a -> Bool) | Placed Index a -- deriving (Eq,Ord)

pick :: Eq a => ((a -> Bool), Index) -> (Jolly a,[a]) -> [(Jolly a,[a])]
pick (k,i) (j,cs) = concat [
		[(j, delete c cs) | c <- filter k cs],
		case j of 
			Given c -> [(Placed i c ,cs)]
			Internal k' -> [(Placed i c, delete c cs) | c <- filter k' cs]
			_ -> []
		]

-- potrebbe essere necessario reversare le condizioni o usare foldl -------------
play :: Eq a => Int -> [a -> Bool] -> (Jolly a,[a]) -> [(Jolly a,[a])]
play i ks jcs = foldr (concatMap . pick) [jcs] $ zip ks [i..]




handleAce x 
	| x == 1 = 14
	| otherwise = x

rankedHandleAce n  = ranked n . first handleAce 

data Result a = R a | N a | E a 
attach :: Game -> Card -> Hand -> [Result (Game,Hand)]

-- attaccare ad un tris
attach t@(Tris ra j) c@(r, _) h
	| r == ra = [R (t,h)] -- same rank
	| (r == 2 || r == -20) && not j = [R (Tris ra True,h)] -- a jolly/pinella 
	| otherwise = []

-- attaccare ad una scala con jolly o pinella all'interno
attach (Scala s (Piazzato r t) (r0,r1)) c@(r', s') h 
	| s /= s' = [] 
	| r == r' && not t = [R (Scala s (Spiazzato t) (r0,r1), h)] -- spiazza il jp che e' di seme diverso
	| r == r' && t && r0 == 3 = [R (Scala s Assente (2,r1), h)] -- la pinella va a finire nel 2  
	| r == r' = [R (Scala s (Spiazzato t) (r0,r1), h)] -- la pinella ha lo stesso seme ma non finisce nel 2
	| r' < r0 = let k None = Scala s (Piazzato r t) (r',r1)
		in map (R .first k) . nubOrdBy snd $ play 0  (map ranked [r' + 1 .. r0 - 1]) (None,h)
	| handleAce r' > r1 = let k None = Scala s (Piazzato r t) (r0, handleAce r') 
		in map (R . first k) . nubOrdBy snd $ play 0 (map ranked [r1 + 1 .. handleAce r' - 1]) (None,h)
	| otherwise = []

-- attaccare ad una scala con jolly o pinella all'esterno
attach (Scala s (Spiazzato t) (r0,r1)) c@(r', s') h 
	| s /= s' = [] 
	| r' < r0 = let 	
		k (Placed n _) = if t && n == 2 then
			Scala s Assente (r', r1)  -- opla la pinella diventa un 2
			else Scala s (Piazzato n t) (r', r1) -- il jolly o pinella viene piazzato
		k (Given _) = if t && r' == 3 then
			Scala s Assente (2,r1) else 	-- opla la pinnella diventa un 2
			Scala s (Spiazzato t) (r', r1) -- il jolly o pinella che sia rimane spiazzato
		in map (R . first k) . nubOrdBy snd $ play r' (map ranked [r' + 1 .. r0 - 1]) (Given undefined, h)
	| handleAce r' > r1 = let  
		k (Placed n _) = Scala s (Piazzato n t) (r0, handleAce r') -- il jolly o pinella viene piazzato
		k (Given _) = Scala s (Spiazzato t) (r0, handleAce r') -- il jolly o pinella che sia rimane spiazzato
		in map (R . first k) . nubOrdBy snd  $ play (r1 + 1) (map ranked [r1 + 1 .. handleAce r' - 1]) (Given undefined, h)
	| otherwise = []

-- attaccare ad una scala che non contiene jolly o pinelle
attach (Scala s Assente (r0,r1))  c@(r', s') h
	| s /= s' && isJP c = [R (Scala s (Spiazzato False) (r0,r1), h)]
	| s /= s' = []
	| r' < r0 = let  
		k (Internal _) = Scala s Assente (r',r1)
		k (Placed n c) = Scala s (Piazzato n (suite c == s)) (r',r1)
		in map (R . first k) . nubOrdBy snd $ play r' (map ranked [r' + 1.. r0 - 1]) (Internal isJP,h)
	| r' > r1 = let 
		k (Internal _) = Scala s Assente (r0,handleAce r')
		k (Placed n c) = Scala s (Piazzato n (suite c == s)) (r0, handleAce r')
		in map (R . first k) . nubOrdBy snd $ play (r1 + 1) (map ranked [r1 + 1 .. handleAce r' - 1]) (Internal isJP,h)
	| otherwise = []

-- attaccare al tavolo ovvero calare una figura nuova
attach Tavolo c@(r, s) h 
	| isJP c = []
	| otherwise = concat $  [t,s0 r ,s1 r ,s2 r]where
		t = let 
			k (Placed _ _) = Tris r True
			k (Internal _) = Tris r False
			in map (N . first k) . nubOrdBy snd $ play 0 (replicate 2 $ ranked r) (Internal isJP, h)
		s0 r' 
			| (r' > 2  || r' == 1) = let 
				r = if r' == 1 then 14 else r' 
				k (Placed n c) = let 
					t = suite c == s
					q = if n == r - 2 then Spiazzato t else Piazzato n t
					in Scala s q (r - 2, r)
				k (Internal _) = Scala s Assente (r - 2,r) 
				in map (N . first k) . nubOrdBy snd $ play (r - 2)
					[ranked (r -2) &.& suited s, ranked (r - 1) &.& suited s] (Internal isJP, h)
			| otherwise = []
		s1 r 
			| r > 1 = let
				k (Placed n c) = Scala s (Spiazzato $ suite c == s) (r -1 , r + 1)
				k (Internal _) = Scala s Assente (r - 1,r + 1) 
				in map (N . first k) . nubOrdBy snd $ play (r - 1)
					[ranked (r - 1) &.& suited s, rankedHandleAce (r + 1) &.& suited s] (Internal isJP, h)
			| otherwise = []
		s2 r 
			| r < 13 = let
				k (Placed n c) = let 
					t = suite c == s
					q = if n == r + 2 then Spiazzato t else Piazzato n t
					in Scala s q (r, r + 2)
				k (Internal _) = Scala s Assente (r, r + 2) 
				in map (N . first k) . nubOrdBy snd $ play r
					[ranked (r + 1) &.& suited s, rankedHandleAce (r + 2) &.& suited s] (Internal isJP, h)
			| otherwise = []
-- attach _ _ _ = []

attach (Scarto cs) c h = [E (Scarto $ c:cs ,h)]



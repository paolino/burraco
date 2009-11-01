{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}
module Burraco where

import Control.Arrow (first)
import Data.List (delete, groupBy, sortBy, find)
import Data.Function (on)
import Control.Monad (liftM2)
import Data.Either (lefts)
import Data.Char (chr)
import Enumeration
import Debug.Trace

newtype Rank = Rank Int deriving (Read,Ord,Eq,Num, Enum)
unRank (Rank x) = x


newtype Suite = Suite Int deriving (Read,Ord,Eq)
instance Show Suite where
	show (Suite y) 
		| y == 0 = "C"
		| y == 1 = "Q"
		| y == 2 = "F"
		| y == 3 = "P"
unSuite (Suite x) = x


newtype Card = Card (Rank, Suite) deriving (Read,Eq, Ord)
unCard (Card x) = x

instance Show Card where
	show (Card (x,y)) 
		| x == -20 = "Jolly"
		| otherwise = show x ++ show y

instance Show Rank where
	show (Rank x) 
		| x == 1 = "A"
		| x == 13 = "K"
		| x == 12 = "Q"
		| x == 11 = "J"
		| x == 14 = "A"
		| otherwise = show $ x
{- 
instance Read Card where
	readsPrec _ "Jolly" = [(Card (-20,-20),"")]
	readsPrec _ (r:s:"") 
		| s == "C" = [Card (
-}
type Hand = [Card]

data PSA = Piazzato Rank Bool  | Spiazzato Bool | Assente deriving (Show,Eq,Ord,Read)
data Game 
	= Tris 
		Rank
		Bool
		
	| Scala 
		Suite
		PSA
		(Rank,Rank)
		
	| Tavolo
	| Scarto [Card] deriving (Eq,Ord,Show,Read)


isScarto (Scarto _) = True
isScarto _ = False

suite =  snd . unCard
jolly = Card (-20,Suite (-20))

isJP = liftM2 (||) (ranked 2) (ranked (-20))


ranked n = (==) n . fst . unCard
rankedWA n = ranked $ case n of 
	Rank 14 ->  Rank 1
	x -> x
suited s = (==) s . suite


nubOrdBy f = map head . groupBy ((==) `on` f) . sortBy (compare `on` f)
nubOrd = nubOrdBy id

x &.& y = liftM2 (&&) x y

subst h x = return . R $ (x,h)
nochance = []
instance CG Game Card where
	attach (Scarto cs) c h = [E (Scarto $ c:cs ,h)]
	attach g (Card (r,s)) h = attach' g (Card (r,s)) h ++ case r of
		1 -> attach' g (Card (14,s)) h
		_ -> nochance

		where
		-- attaccare ad un tris
		attach' t@(Tris ra j) c@(Card (r, _)) h
			| r == ra = subst h t -- same rank
			| (r == 2 || r == -20) && not j = subst h $ Tris ra True -- a jolly/pinella 
			| otherwise = nochance

		-- attaccare ad una scala con jolly o pinella all'interno
		attach' (Scala s (Piazzato r t) (r0,r1)) c@(Card (r', s')) h 
			| s /= s' = nochance 
			| r == r' && t && r0 == Rank 3 = subst h $ Scala s Assente (Rank 2,r1)-- la pinella va a finire nel 2  
			| r == r' = subst h $ Scala s (Spiazzato t) (r0,r1) -- la pinella ha lo stesso seme ma non finisce nel 2
			| r' == r0 - Rank 1 = subst h $ Scala s (Piazzato r t) (r',r1)
			| r' == r1 + Rank 1 = subst h $ Scala s (Piazzato r t) (r0,  r')
			| otherwise = nochance

		-- attaccare ad una scala con jolly o pinella all'esterno
		attach' (Scala s (Spiazzato t) (r0,r1)) c@(Card (r', s')) h 
			| s /= s' = nochance 
			| r' == r0 - Rank 1 = subst h $ Scala s (Spiazzato t) (r',r1)
			| r' == r0 - Rank 2 = subst h $ Scala s (Piazzato (r0 - Rank 1) t) (r',r1)
			| r' == r1 + Rank 1 = subst h $ Scala s (Spiazzato t) (r0, r') 
			| r' == r1 + Rank 2 = subst h $ Scala s (Piazzato (r0 + Rank 1) t) (r', r1)
			| otherwise = nochance

		-- attaccare ad una scala che non contiene jolly o pinelle
		attach' (Scala s Assente (r0,r1))  c@(Card (r', s')) h
			| s /= s' && isJP c = subst h $ Scala s (Spiazzato False) (r0,r1)
			| s /= s' = nochance
			| r' == r0 - Rank 1 = subst h $ Scala s Assente (r',r1)  
			| r' == r1 + Rank 1 = subst h $ Scala s Assente (r0, r') 
			| otherwise = nochance

		-- attaccare al tavolo ovvero calare una figura nuova
		attach' Tavolo c@(Card (r, s)) h 
			| isJP c = nochance
			| otherwise = let 
				block k = map (N . first k) . nubOrdBy snd
				checks s = zipWith (&.&) (repeat $ suited s) 
				t = let 	k (Right _) = Tris r True
						k _ = Tris r False
						in block k  $ picks (Rank 0) (replicate 2 $ ranked r) (Left isJP, h)
				s0 r	| r > Rank 2 = let 	
						k (Right (n,c)) = let 
							t = suite  c == s
							in uncurry (Scala s) $ case n == r - Rank 2 of
								True -> (Spiazzato t, (r - Rank 1, r))
								_  -> (Piazzato n t, (r - Rank 2, r))
						k _ = Scala s Assente (r - Rank 2,r) 
						in block k $ picks (r - Rank 2) (checks s [ranked (r - Rank 2), ranked (r - Rank 1)]) (Left isJP, h)
					| otherwise = nochance
				s1 r 	| (r >  Rank 1 && r <  Rank 14) = let
						k (Right  (n,c)) = Scala s (Spiazzato $ suite c == s) $ case n == r - Rank  1 of
							True -> (r , r +  Rank 1)
							_ -> (r -  Rank 1, r)
						k _ = Scala s Assente (r -  Rank 1,r +  Rank 1) 
						in block k $ picks (r -  Rank 1) (checks s [ ranked (r -  Rank 1), rankedWA (r +  Rank 1)]) (Left isJP, h)
					| otherwise = nochance
				s2 r 	| r <  Rank 13 = let
						k (Right (n,c)) = let 
							t = suite c == s
							in uncurry (Scala s) $ case n == r +  Rank 2 of
								True  -> (Spiazzato t, (r, r +  Rank 1))
								_ -> (Piazzato n t, (r, r +  Rank 2))
						k _ = Scala s Assente (r, r +  Rank 2) 
						in block k  $ picks (r +  Rank 1) (checks s [ranked (r +  Rank 1) , rankedWA (r +  Rank 2)])  (Left isJP, h)
					| otherwise = nochance
				in concat [t,s0 r ,s1 r ,s2 r] 

type Jolly a = Either (a -> Bool) (Rank, a) -- deriving (Eq,Ord)

pick :: Eq a => ((a -> Bool), Rank) -> (Jolly a,[a]) -> [(Jolly a,[a])]
pick (k,i) (j,cs) = concat [
		[(j, delete c cs) | c <- filter k cs],
		case j of 
			Left k' -> [(Right (i, c), delete c cs) | c <- filter k' cs]
			_ -> []
		]

-- potrebbe essere necessario reversare le condizioni o usare foldl -------------
picks :: Eq a => Rank -> [a -> Bool] -> (Jolly a,[a]) -> [(Jolly a,[a])]
picks i ks jcs = foldr (concatMap . pick) [jcs] $ zip ks [i..]


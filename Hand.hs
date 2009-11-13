module Hand where

import Data.Map as M
import Prelude as P
import Data.List (inits)
import Card

data Q = SequenzeQ {unSQ :: Map Suite (Map Rank Int)} | TrisQ {unTQ :: Map Rank (Map Suite Int)}

delete :: Card -> Q -> Q
delete (Card (r,s)) (TrisQ m) = TrisQ $ update (Just . update (Just . subtract 1) s ) r m
delete (Card (r,s)) (SequenzeQ m) = SequenzeQ $ update (Just . update (Just . subtract 1) r ) s m

comb :: [a] -> [[a]]
comb [] = return []
comb (x:xs) = let cs = comb xs in P.map (x:) cs ++ cs  

data Hand = Hand 
	Q -- sequenze
	Q -- tris
	Int -- eventuali jolly


	

combinazioni  :: Hand -> [(Combination, Hand)]
combinazioni = undefined

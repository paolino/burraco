{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Applicative (liftA2)
import Data.List (groupBy, sortBy, sort)
import Data.Function (on)
import System.Random
import Control.Monad.State
-- import Data.Ord (comparing)

type Rank = Int
type Suite = Int

data Card 
	= Card Rank Suite
	| Jolly deriving (Eq,Ord,Show)

rank Jolly = -20
rank (Card r _) = r

suite Jolly = -20
suite (Card _ s) = s

mkCard :: (Int, Int) -> Card
mkCard = uncurry Card

data PSA = Piazzato Rank Bool  | Spiazzato Bool | Assente
data Game 
	= Tris {
		grank :: Rank,
		gjolly :: Bool
		}
	| Scala {
		gsuite :: Suite,
		gmatta :: (PSA,Bool)
		gbirank :: (Rank,Rank)
		}
	| Tavolo 
	| Scarto

data UseJolly = NoJ | MioJ | TuoJ

workout :: Suite -> (Rank,Rank) -> UseJolly  -> Hand -> [(Maybe (Rank, Bool),Hand)]
workout = undefined

attach :: Game -> Card -> Hand -> [(Game,Hand)]
attach t@(Tris ra j) c@(Card r _) h
	| r == ra = [(t,h)]
	| r == 2 && not j = [(Tris ra True,h)]
	| otherwise = []
attach (Tris ra False) Jolly h = [(Tris ra True,h)]
attach (Tris ra True) Jolly h = []

attach (Scala s (Piazzato r t) (r0,r1)) c@(Card r' s') h 
	| s /= s' = [] 
	| r == r' && not t = [(Scala s (Spiazzato t) (t0,t1), h)]
	| r == r' && t && t0 == 3 = [(Scala s Assente (2,t1), h)]
	| r == r' = [(Scala s (Spiazzato t) (t0,t1), h)]
	| r' < r0 = map (first . const $ Scala s (Piazzato r t) (r',r1)) $ workout (r',r0) NoJ h
	| r' > r1 = map (first . const $ Scala s (Piazzato r t) (r0,r')) $ workout (r1,r') NoJ h
	| otherwise = []

attach (Scala s (Piazzato r _) (r0,r1)) Jolly h = []

attach (Scala s (Spiazzato t) (r0,r1)) c@(Card r' s') h 
	| s /= s' = [] 
	| r' < r0 = flip map (workout (r',r0) MioJ h) . first $ \n -> case n of
		Nothing -> if t && r' == 3 then
			Scals s Assente (2,r1) else
			Scala s (SPiazzato t) (r', r1)
		Just (n,_) -> if t && n == 2 then
			Scala s Assente (r', r0)
			else Scala s (Piazzato n t) (r', r1)
	| r' < r0 = flip map (workout (r1,r') MioJ h) . first $ \n -> case n of
		Nothing -> Scala s (SPiazzato t) (r0, r')
		Just (n,_) -> Scala s (Piazzato n t) (r0, r')
	| otherwise = []

attach (Scala s (SPiazzato _) (r0,r1)) Jolly h = []

attach (Scala s Assente (r0,r1)) Jolly h = [(Scala s (Spiazzato False) (r0,r1), h)]

attach (Scala s Assente (r0,r1))  c@(Card r' s') h
	| s /= s' && r' == 2 = [(Scala s (Spiazzato False) (r0,r1), h)]
	| s /= s' = []
	| r' < r0 = flip map (workout (r',r0) TuoJ h) . first $ \n -> case n of
		Nothing -> Scala s Assente (r',r1)
		Just (n,t) -> Scala s (Piazzato n t) (r',r1)
	| r' > r1 = flip map (workout (r1,r') TuoJ h) . first $ \n -> case n of
		Nothing -> Scala s Assente (r0,r')
		Just (n,t) -> Scala s (Piazzato n t) (r0,r')
	| otherwise = []


attach Tavolo Jolly h = []
attach Tavolo c@(Card r' s') (Hand cs) 
	| r' == 2 = []
	| otherwise = let scs = filter (\x -> suite x == s) cs in
		concat [	
		do
			r1 <- nub $ filter (\x -> rank x == r') cs
			let cs1 = delete r1 cs
			r2 <- nub $ filter (\x -> rank x `elem` [-20,2,r']) cs1
			return (Tris r' False, Hand $ delete r2 cs1) 
		, do 	
			r1 <- maybeToList $ find (\x -> rank x = r'- 2) scs
			
		]
			
		hs = pick h
		
pick :: (Card -> Bool) -> Hand -> [Hand
pick cond (Hand cs) = case find cond cs of
	Just r ->  let h = Hand $ delete r cs in tell [h] >> pick cond h
	Nothing -> return ()
		

	| r == 2 && lenght cs < 13 = \h -> [(Scala su (c:cs),h)]
	| s == su && not (c `elem` cs) = \h ->
		
----------------- tests -------------------------------

deck = concat . replicate 2 $ [Card i j | i <- [1..13] , j <- [0..3]] ++ replicate 4 Jolly

pickT = do
	rs <- get
	n <- (`mod` length rs) `fmap` lift randomIO
	put $ take n rs ++ drop (n + 1) rs
	return $ rs !! n

handT n  = mkHand `fmap` replicateM n pickT

runT f = evalStateT f deck


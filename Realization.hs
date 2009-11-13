module Realization 
	where
import Control.Arrow (second, (&&&))
import Data.List ((\\),sortBy, groupBy , sort, partition, group)
import Data.Ord (comparing)
import Card (Card (..), ranked, suited, Rank, Suite)
import Data.Function (on)
import Control.Monad (ap)

data Burraco = Scala Suite Rank | Tris Rank deriving (Show, Eq,Ord)

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x
rankOf (Card (r,s)) = r

extractScala f r rs = let 
	(r1,r1') = break ((>= r) . f) rs
	(r2,r2') = break ((>= r + 7) . f) r1'
	in (r2, r1 ++ r2')  
	
nubRemind :: (Ord b, Eq a) => (a -> b) -> [a] -> ([a],[a])
nubRemind f = second concat . unzip . map (head &&& tail) . group . sortBy (comparing f)

calata :: [Burraco] -> [Card] -> ([Card],[[Card]])
calata bs cs = let (r:rs) = scanr (\b (rs,_,cs) -> let (rs',q,cs') = valutazione0 b rs in (rs' \\cs',q,cs')) (zip cs (repeat 0),undefined,[]) bs
	in  (map fst (fst3 $ r) , map (map fst . thd3) (init rs))

valutazione0 :: Burraco -> [(Card,Float)] -> ([(Card,Float)],Float,[(Card, Float)])
valutazione0 (Tris r) cs = let 
	(rs,ts) = partition (ranked r . fst) $ cs  
	rss = map (second (+ (7 / fromIntegral (length rss)))) rs
	q = sum . map ((1/) . snd) $ rs
	in (rss ++ ts ++ ts, q, rss)
valutazione0 (Scala s r) cs = let 
	(ss,ts) = partition (suited s . fst) cs
	(ss1,ss2) = extractScala (rankOf . fst) r ss
	ss3 = groupBy ((==) `on` fst) ss1
	q = sum . map ((1/) . snd) $ ss1
	in (concatMap (\xs -> map (second  (+ (1 / fromIntegral (length xs)))) xs ) ss3 ++ ss2 ++ ts, q, ss1)
	

spazio :: [Card] -> [Burraco] -> [(Card,Float)]
spazio cs bs = foldr ((fst3 .) . valutazione0) (zip cs (repeat 0)) (sort bs)

ordinamento :: [Card] -> [Burraco] -> [Burraco]
ordinamento cs bs = let 
	s = spazio cs bs 
	in map fst . sortBy (flip . comparing $ snd ) $  ap zip (map $ snd3 . flip valutazione0 s) bs
---------- test ----------------



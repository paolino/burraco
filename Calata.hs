{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Card
import Data.List
import Data.Ord
import Control.Arrow
import Control.Parallel.Strategies
import Control.Parallel

newtype ML = ML [Card] deriving (NFData, Show)
size (ML x) = length x
fromList = ML . sort
toList (ML x) = x

intersection' [] _ xs = xs
intersection'  _ [] xs = xs
intersection' (x:xs) (y:ys) zs
	| y == x = intersection' xs ys (x:zs)
	| y < x = intersection' (x:xs) ys zs
	| y > x = intersection' xs ys zs

intersection :: ML -> ML -> ML 
intersection (ML xs) (ML ys) = fromList $ intersection' xs ys []

difference' [] _ = []
difference' xs [] = xs
difference' (x:xs) (y:ys)
	| x == y = difference' xs ys
	| x < y = x:difference' xs (y:ys)
	| x > y = difference' (x:xs) ys

difference :: ML -> ML -> ML
difference (ML xs) (ML ys) = ML $ difference' xs ys
-- | compute the subsets of a set, including the empty one (run in exponential space, care)
powerset []     = [[]]
powerset (x:xs) = xss ++ map (x:) xss
	    where xss = powerset xs

type Gioco = ML 
scale = map fromList $ do
	j <- [0 .. 3]
	n <- [1 .. 13]
	x <- [1 .. 14 - n]
	return . map (Card . flip (,) j) $ [x .. x + n] 
		
trisses = let 	f j = concatMap $ \x -> [x] ++  if j `elem` x then [j:x] else []
	in map fromList $ do
		n <- [1 .. 13]
		x<- foldr f (tail . powerset $ [0 .. 3]) [0..3]
		return . map (Card . (,) n) $ x


giochi :: [Gioco]
giochi = scale ++ trisses  

type Intersezione = ML
type Dotazione = ML

-- | tabella delle intersezioni per ogni gioco, la lista di intersezioni del gioco stesso con la dotazione privata di tutti i giochi, a turno
intable :: Dotazione  -> [[Intersezione]]
intable xs = let 	ws = map (difference xs) giochi -- mani decurtate
		in   parMap rnf (\g -> map (intersection (intersection g xs)) ws) giochi -- intersezioni di tutti i giochi con le mani decurtate



type Q =  Gioco -> [Intersezione] -> Float 

classifica :: Q -> [[Intersezione]] -> [(Gioco,Float)]
classifica q ts = let 
	by = sortBy (flip $ comparing snd) 
	in by . zip giochi . parMap rnf  (uncurry q) $ zip giochi ts

simpleEI g = sum . map (\i -> fromIntegral (size i) / (fromIntegral (size g) ** (0.7 :: Float)))

main = do
	h <- fromList `fmap` runT (handT 11)
	print h
	let t = intable h
	mapM_ print  $ classifica simpleEI  t 
		

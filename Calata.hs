import Card
import Data.List
import Data.Ord
import Control.Arrow
import Control.Parallel.Strategies
import Control.Parallel
import Data.Set hiding (map)

-- | compute the subsets of a set, including the empty one (run in exponential space, care)
powerset []     = [[]]
powerset (x:xs) = xss ++ map (x:) xss
	    where xss = powerset xs

type Gioco = Set Card
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

bests m xs = let 	ws = map (difference xs) giochi
			ys g = sum $ map (m g) ws
			byf f g = map g . sortBy (flip $ comparing f) 
		in byf snd id  . zip giochi . (pseq $ rnf ws) . parMap rnf  ys $ giochi

simpleEI d g = fromIntegral (size $ intersection d g) / (fromIntegral (size g) ** (0.7 :: Float))

main = do
	h <- fromList `fmap` runT (handT 11)
	print h
	mapM_ print . take 20 $ bests simpleEI h 
		

{-# LANGUAGE GeneralizedNewtypeDeriving,DeriveFunctor #-}
import Card
import Data.List
import Data.Ord
import Control.Arrow
import Control.Parallel.Strategies
import Control.Parallel
import Data.Maybe
import Control.Monad
import System.Environment

newtype ML a = ML a deriving (NFData, Show, Functor)
type MLc = ML (Int,[Card])
size (ML (i,_)) = i
fromList i x  = ML (i,sort x)
toList (ML (_,x)) = x
empty (ML (0,_)) = True
empty _ = False

insertion :: Card -> MLc -> MLc
insertion c (ML (i,xs)) = ML (i + 1,bs ++ c:as) where
	(bs,as) = break (> c) xs

intersection' [] _ x = x
intersection'  _ [] x = x
intersection' (x:xs) (y:ys) r@(ML (i,zs))
	| y == x = intersection' xs ys (ML (i + 1, x:zs))
	| y < x = intersection' (x:xs) ys r
	| y > x = intersection' xs (y:ys) r

intersection :: MLc -> MLc -> MLc
intersection (ML (_,xs)) (ML (_,ys)) = (id *** reverse) `fmap` intersection' xs ys (ML (0,[]))

difference (ML (_,[])) _  = ML (0,[])
difference x (ML (_,[])) = x
difference mx@(ML (ix,(x:xs))) my@(ML (iy,(y:ys)))
	| x == y = difference (ML (ix -1,xs)) (ML (iy - 1,ys))
	| x < y = fmap ((+1) *** (x:)) $ difference (ML (ix -1,xs)) my
	| x > y = difference mx (ML (iy - 1,ys))


-- | compute the subsets of a set, including the empty one (run in exponential space, care)
powerset []     = [[]]
powerset (x:xs) = xss ++ map (x:) xss
	    where xss = powerset xs

type Gioco = MLc 
scale = map (ap (flip fromList) length)$ do
	j <- [0 .. 3]
	n <- [1 .. 13]
	x <- [1 .. 14 - n]
	return . map (flip mkCard j) $ [x .. x + n] 
		
trisses = let 	f j = concatMap $ \x -> [x] ++  if j `elem` x then [j:x] else []
	in map (ap (flip fromList) length) $ do
		n <- [1] ++ [3 .. 13]
		x<- foldr f (tail . powerset $ [0 .. 3]) [0..3]
		return . map (mkCard n) $ x


giochi :: [Gioco]
giochi = scale ++ trisses  

type Intersezione = MLc
type Dotazione = MLc

-- | tabella delle intersezioni per ogni gioco, la lista di intersezioni del gioco stesso con la dotazione privata di tutti i giochi, a turno
intable :: Dotazione  -> [Gioco] -> [(Gioco,[Intersezione])]
intable xs ys = let 	ws = map (difference xs) ys -- mani decurtate
			f g = let z =  intersection g xs in
				if empty z then []
				else [(g,map (intersection z) ws)]
		in   concat $ parMap rnf f ys -- intersezioni di tutti i giochi con le mani decurtate



type Q =  Gioco -> [Intersezione] -> Float 

classifica :: Q -> [(Gioco,[Intersezione])] -> [(Gioco,Float)]
classifica q = sortBy (flip $ comparing snd) . parMap rnf  (fst &&& uncurry q) 

simpleQ k g = (* (fromIntegral (size g) ** (-k))) . fromIntegral . sum . map size 

type R = Dotazione -> Gioco -> Maybe Gioco
simpleR :: R
simpleR x y = let 
	i = intersection x y
	d = difference x y
	hack = head $ toList y
	in case size y - size i of
		0 -> Just  y
		1 -> let 	rs = filter isJP . toList $ d
			 	q = find (suitedlike hack) rs `mplus` listToMaybe rs
				in flip insertion i `fmap` q				
		_ -> Nothing

tavoloR :: R
tavoloR x y = simpleR x y >>= \x -> if (size x < 3) then Nothing else Just x
calata' :: (Q,R) -> [Gioco] -> Dotazione -> Maybe ([Gioco],Gioco)
calata' _ _ (ML (0,_))  = Nothing
calata' (q,r) ys x = case classifica q (intable x ys) of 
				[] -> Nothing
				rs -> (,) (map fst rs) `fmap` (msum $ map (r x . fst) rs)
   
calata :: (Q,R) -> [Gioco] -> Dotazione -> [Gioco]
calata q _ (ML (0,_))  = []
calata q ys x = case calata' q ys x of 
				Nothing -> []
				Just (rs,y) -> y:calata q rs (difference x y)     



make n = fromList n `fmap` runT (handT n)

main = do
	(c:p:_) <- getArgs
	h <- make (read c)
	putStrLn "----- Dotazione --------------"
	print (toList h)
	putStrLn "----- Giochi generici --------"
	mapM_ (print . toList)  $ calata (simpleQ (read p),simpleR) giochi h
	putStrLn "----- Giochi realizzabili ----"
	mapM_ (print . toList)  $ calata (simpleQ (read p),tavoloR) giochi h
		

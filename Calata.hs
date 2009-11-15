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

-------------- library for a set of cards ----------------------------------

-- | generic set with support for element molteplicity
newtype Set a = Set (Int,[a]) deriving (NFData, Show)

unsafeCons :: a -> Set a -> Set a
unsafeCons x (Set (i,xs)) = Set (i + 1, x : xs)
-- | extract number of elements
size :: Set a -> Int
size (Set (i,_)) = i

-- | care to build a valid Set from a list (not very efficient for small sets)
fromList :: Ord a => [a] -> Set a 
fromList x = Set (length x,sort x)

-- | extract the list of elements from the Set
toList :: Set a -> [a]
toList (Set (_,x)) = x

-- | test for emptyness
empty :: Set a -> Bool
empty (Set (0,_)) = True
empty _ = False

-- | element insertion
insertion :: Ord a => a -> Set a -> Set a
insertion c (Set (i,xs)) = Set (i + 1,bs ++ c:as) where
	(bs,as) = break (> c) xs

-- | sets intersection
intersection :: Ord a => Set a -> Set a -> Set a
intersection s@(Set (0,[])) _  = s
intersection _ s@(Set (0,[])) = s
intersection mx@(Set (ix,(x:xs))) my@(Set (iy,(y:ys)))
	| x == y = unsafeCons x $ intersection (Set (ix -1,xs)) (Set (iy - 1,ys))
	| x < y = intersection (Set (ix -1,xs)) my
	| x > y = intersection mx (Set (iy - 1,ys))

-- | sets difference
difference :: Ord a => Set a -> Set a -> Set a
difference s@(Set (0,[])) _  = s
difference x (Set (_,[])) = x
difference mx@(Set (ix,(x:xs))) my@(Set (iy,(y:ys)))
	| x == y = difference (Set (ix -1,xs)) (Set (iy - 1,ys))
	| x < y = unsafeCons x $ difference (Set (ix -1,xs)) my
	| x > y = difference mx (Set (iy - 1,ys))


------------- End of Set library (should be called Bag?) ----------------
--------- Powerset library ------------------------

-- | compute the subsets of a set, including the empty one (run in exponential space, care)
powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = xss ++ map (x:) xss
	    where xss = powerset xs
-------------- End of powerset library --------------------------
-------------- Definizione dei giochi ------------------
-- | specific Set of cards
type Cardset = Set Card

-- | chiamiamo gioco un insieme di carte che rappresenta un gioco teorico come "la scala di cuori dal 4 al 9" etc
type Gioco = Cardset

-- | tutte le scale possibili 
scale :: [Cardset]
scale = map fromList $ do
	j <- [0 .. 3]
	n <- [1 .. 13]
	x <- [1 .. 14 - n]
	return . map (flip mkCard j) $ [x .. x + n] 
		
-- | tutti i tris possibili
trisses :: [Cardset]
trisses = let 	f j = concatMap $ \x -> [x] ++  if j `elem` x then [j:x] else []
	in map fromList $ do
		n <- [1] ++ [3 .. 13]
		x<- foldr f (tail . powerset $ [0 .. 3]) [0..3]
		return . map (mkCard n) $ x

-- | tutti i giochi possibili
giochi :: [Gioco]
giochi = scale ++ trisses  

-------------------------- 
-- | Chiamiamo intersezione le carte in dotazione che fanno parte di un gioco 
type Intersezione = Cardset

-- | Chiamiamo dotazione le carte che si trovano in mano
type Dotazione = Cardset

-- | tabella delle intersezioni per ogni gioco, la lista di intersezioni del gioco stesso con la dotazione privata di tutti i giochi, a turno. La tabella produce solo le righe dei giochi che hanno intersezione non nulla con la dotazione. Le intersezioni sono esguite in parallelo
intable :: Dotazione  -> [Gioco] -> [(Gioco,[Intersezione])]
intable xs ys = let 	ws = map (difference xs) ys -- mani decurtate
			f g = let z =  intersection g xs in
				if empty z then []
				else [(g,map (intersection z) ws)]
		in   concat $ parMap rnf f ys -- intersezioni di tutti i giochi con le mani decurtate


-- | plugin per la valutazione dei giochi, riceve il gioco da valutare e le intersezioni con la dotazione, decurtata a turno da tutti i giochi possibili
type Q =  Gioco -> [Intersezione] -> Float 

-- | forma un sottoinsieme ordinato per importanza di tutti i giochi, partendo dalla tabella delle intersezioni. Viene eseguita in parallelo
classifica :: Q -> [(Gioco,[Intersezione])] -> [Gioco]
classifica q = map fst . sortBy (flip $ comparing snd) . parMap rnf  (fst &&& uncurry q) 

-- | una implementazione classica del plugin Q. Somma la grandezza delle intersezioni e le pesa con una funzione della lunghezza teorica del gioco
simpleQ k g = (* (fromIntegral (size g) ** (-k))) . fromIntegral . sum . map size 

-- | plugin per la determinazione di realizzabilità di un gioco. Passa potenzalmente da un gioco teorico ad uno realizzato
type R = Dotazione -> Gioco -> Maybe Intersezione


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
tavoloR x y = do 
	x <- simpleR x y 
	guard $ size x >= 3
	return x 

calata' :: (Q,R) -> ([Gioco],Dotazione) -> Maybe (Intersezione,([Gioco],Dotazione))
calata' _  (_,(Set (0,_)))  = Nothing
calata' (q,r) (ys,x) = case classifica q (intable x ys) of 
				[] -> Nothing
				rs -> do 	y <- msum $ map (r x) rs -- gioco scelto
						return (y,(rs,difference x y))
					
calata :: (Q,R) -> ([Gioco],Dotazione) -> [Intersezione]
calata = unfoldr . calata'    



make n = fromList `fmap` runT (handT n)

main = do
	(c:p:_) <- getArgs
	h <- make (read c)
	putStrLn "----- Dotazione --------------"
	print (toList h)
	putStrLn "----- Giochi generici --------"
	mapM_ (print . toList)  $ calata (simpleQ (read p),simpleR) (giochi,h)
	putStrLn "----- Giochi realizzabili ----"
	mapM_ (print . toList)  $ calata (simpleQ (read p),tavoloR) (giochi,h)
		

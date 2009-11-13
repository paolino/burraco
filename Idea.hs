{-# LANGUAGE FunctionalDependencies, GADTs , KindSignatures, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
import Card
import Data.List
import Data.Function
import Data.Ord
import Data.Maybe
import Control.Arrow
import qualified Data.Map as M




type Index = Int
type Combinazione a = [(a,Index)] -> [Index]
type Idea a = [Combinazione a]


contribute :: [a] -> Combinazione a -> [(Index,Float)]
contribute ws c =
	let	rs = c (zip ws [0..] ) :: [Index]
		h0 = 1 / fromIntegral (length rs) :: Float
	in zip rs (repeat h0)


type Interference = M.Map Index Float
interference :: Ord a => [a] -> Idea a -> Interference 
interference xs cs = let 
	zs = map (fst . head &&& sum . map snd) . groupBy ((==) `on` fst) 
		. sortBy (comparing fst) . concatMap (contribute xs) $ cs
	in M.unionWith (+) (M.fromList (zip [0 .. length xs - 1] (repeat 0))) (M.fromList zs)

score :: [a] -> Interference  -> Combinazione a -> Float
score ws i c = sum . map (i M.!) $ c (zip ws [0..] )

interest :: Ord a => [a] -> Idea a -> Float
interest  ws cs = sum . map (score ws . interference ws $ cs) $ cs  
		
		
		
------------- combinations topology ------


data EB :: * -> * where
	EB :: ElementC a b => a -> EB b

class ElementC a b where
	neighbours :: a -> [EB b]
	value :: a -> b -> Float
	
	
expand :: EB b -> [EB b]
expand (EB y) = neighbours y  
	
data Seq = Seq Suite (Rank, Rank)

data Tri = Tri Rank 

instance ElementC Seq [Card] where
	neighbours (Seq s (r1,r2)) = [EB (Seq s (r1 +1, r2 +1)), EB (Tri r1)] 

instance ElementC Tri [Card] where
	neighbours (Tri r) = [EB (Seq 0 (r,r+1))]

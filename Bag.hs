{-# LANGUAGE GeneralizedNewtypeDeriving,ScopedTypeVariables,TypeSynonymInstances, DeriveFunctor #-}

module Bag (
	Bag,
	size,
	fromList,
	toList,
	empty,
	insertion,
	difference,
	intersection,
	union,
	unions
	)
	where

import Test.QuickCheck 
import Control.Parallel.Strategies (NFData)
import Data.List (sort)

-------------- library for a set of cards ----------------------------------

-- | generic set with support for element molteplicity
newtype Bag a = Bag (Int,[a]) deriving (NFData, Show, Eq, Ord)

unsafeCons :: a -> Bag a -> Bag a
unsafeCons x (Bag (i,xs)) = Bag (i + 1, x : xs)
-- | extract number of elements
size :: Bag a -> Int
size (Bag (i,_)) = i

-- | care to build a valid Bag from a list (not very efficient for small sets)
fromList :: Ord a => [a] -> Bag a 
fromList x = Bag (length x,sort x)

-- | extract the list of elements from the Bag
toList :: Bag a -> [a]
toList (Bag (_,x)) = x

-- | test for emptyness
empty :: Bag a -> Bool
empty (Bag (0,_)) = True
empty _ = False

-- | element insertion
insertion :: Ord a => a -> Bag a -> Bag a
insertion c (Bag (i,xs)) = Bag (i + 1,bs ++ c:as) where
	(bs,as) = break (> c) xs

-- | sets intersection
intersection :: Ord a => Bag a -> Bag a -> Bag a
intersection s@(Bag (0,[])) _  = s
intersection _ s@(Bag (0,[])) = s
intersection mx@(Bag (ix,(x:xs))) my@(Bag (iy,(y:ys)))
	| x == y = unsafeCons x $ intersection (Bag (ix -1,xs)) (Bag (iy - 1,ys))
	| x < y = intersection (Bag (ix -1,xs)) my
	| x > y = intersection mx (Bag (iy - 1,ys))

-- | sets difference
difference :: Ord a => Bag a -> Bag a -> Bag a
difference s@(Bag (0,[])) _  = s
difference x (Bag (_,[])) = x
difference mx@(Bag (ix,(x:xs))) my@(Bag (iy,(y:ys)))
	| x == y = difference (Bag (ix -1,xs)) (Bag (iy - 1,ys))
	| x < y = unsafeCons x $ difference (Bag (ix -1,xs)) my
	| x > y = difference mx (Bag (iy - 1,ys))


union :: Ord a => Bag a -> Bag a -> Bag a
union (Bag (0,[])) y = y
union x (Bag (0,[])) = x
union mx@(Bag (ix,(x:xs))) my@(Bag (iy,(y:ys)))
	| x == y = unsafeCons y . unsafeCons x $ union (Bag (ix -1,xs)) (Bag (iy - 1,ys))
	| x < y = unsafeCons x $ union (Bag (ix -1,xs)) my
	| x > y = unsafeCons y $ union mx (Bag (iy - 1,ys))

unions :: Ord a => [Bag a] -> Bag a
unions [] = error "Bag:unions of no sets"
unions x = foldr1 union x

-------------- Test -------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (Bag a) where
	arbitrary = fromList `fmap` listOf arbitrary


type BI = Bag Int
prop_coherent (Bag (i,x) :: BI) = length x == i && sort x == x

prop_unionSL (x :: BI)  y= prop_coherent $ union x y 

prop_intersectionSL (x::BI) y = prop_coherent $ intersection x y

prop_differenceSL (x::BI) y = prop_coherent $ difference x y

prop_insertionSL (x::Int) y = prop_coherent $ insertion x y

prop_difference2isempty (x::BI) = empty $ difference x x

prop_uniondifferenceintersection (x::BI) y =  let z = x `union` y in 
	z == union (difference z y) (intersection z y)

main = do
	print "Bag tests............"
	quickCheck prop_coherent
	quickCheck prop_unionSL
	quickCheck prop_intersectionSL
	quickCheck prop_differenceSL
	quickCheck prop_difference2isempty
	quickCheck prop_insertionSL
	quickCheck prop_uniondifferenceintersection
 





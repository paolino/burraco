{-# LANGUAGE GeneralizedNewtypeDeriving,FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables,TypeSynonymInstances, DeriveFunctor #-}

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
	unions,
	isSubsetOf,
	subsetGen,
	subsetGen1
	)
	where

import Test.QuickCheck 
import Control.Parallel.Strategies (NFData)
import Data.List (sort, delete)

-------------- library for a set of cards ----------------------------------

-- | generic set with support for element molteplicity
newtype Bag a = Bag (Int,[a]) deriving (NFData, Show, Eq, Ord, Read)

-- | this can break coherence, see Testable instance
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

-- | test for emptyness of a bag
empty :: Bag a -> Bool
empty (Bag (0,_)) = True
empty _ = False

-- | element insertion
insertion :: Ord a => a -> Bag a -> Bag a
insertion c (Bag (i,xs)) = Bag (i + 1,bs ++ c:as) where
	(bs,as) = break (> c) xs

-- | bags intersection
intersection :: Ord a => Bag a -> Bag a -> Bag a
intersection s@(Bag (0,[])) _  = s
intersection _ s@(Bag (0,[])) = s
intersection mx@(Bag (ix,(x:xs))) my@(Bag (iy,(y:ys)))
	| x == y = unsafeCons x $ intersection (Bag (ix -1,xs)) (Bag (iy - 1,ys))
	| x < y = intersection (Bag (ix -1,xs)) my
	| x > y = intersection mx (Bag (iy - 1,ys))

-- | bags difference
difference :: Ord a => Bag a -> Bag a -> Bag a
difference s@(Bag (0,[])) _  = s
difference x (Bag (_,[])) = x
difference mx@(Bag (ix,(x:xs))) my@(Bag (iy,(y:ys)))
	| x == y = difference (Bag (ix -1,xs)) (Bag (iy - 1,ys))
	| x < y = unsafeCons x $ difference (Bag (ix -1,xs)) my
	| x > y = difference mx (Bag (iy - 1,ys))

-- | bags union
union :: Ord a => Bag a -> Bag a -> Bag a
union (Bag (0,[])) y = y
union x (Bag (0,[])) = x
union mx@(Bag (ix,(x:xs))) my@(Bag (iy,(y:ys)))
	| x == y = unsafeCons y . unsafeCons x $ union (Bag (ix -1,xs)) (Bag (iy - 1,ys))
	| x < y = unsafeCons x $ union (Bag (ix -1,xs)) my
	| x > y = unsafeCons y $ union mx (Bag (iy - 1,ys))

-- | unions of a list of bags
unions :: Ord a => [Bag a] -> Bag a
unions [] = error "Bag:unions of no sets"
unions x = foldr1 union x

-- | test if the first bag is subset of the second
isSubsetOf :: Ord a => Bag a -> Bag a -> Bool
x `isSubsetOf` y = x == intersection x y 

---------------------------------------------------------------------------------------------------------------
-------------- Arbitrary instance , Testable instance and some generators -------------------------------------
---------------------------------------------------------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (Bag a) where
	arbitrary = fromList `fmap` arbitrary

instance Ord a => Testable (Bag a) where
	property  (Bag (i,x)) = property $ length x == i && sort x == x

-- | subset generator. complexity bug: uses List.delete, fix: use a Data.Set.remove
subsetGen :: Ord a => Bag a -> Gen (Bag a) 
subsetGen x = do
	n <- elements [0 .. size x]
	fromList `fmap` subset (toList x) n
	where
	subset x 0 = return []
	subset x n = do 
		r <- elements x
		rs <- subset (delete r x) (n -1)
		return $ r:rs

-- | not empty subset generator
subsetGen1 :: Ord a => Bag a -> Gen (Bag a) 
subsetGen1 x = subsetGen x `suchThat` (not . empty)


------------------------------------------------------------------------------------------------
---------------------------- test suite on Bag Int ---------------------------------------------
------------------------------------------------------------------------------------------------

type BI = Bag Int

prop_unionSL x y = return $ union x y :: Gen BI

prop_intersectionSL x y = return $ intersection x y :: Gen BI

prop_differenceSL x y = return $ difference x y :: Gen BI

prop_insertionSL x y = return $ insertion x y :: Gen BI

prop_difference2isempty (x::BI) = empty $ difference x x

prop_uniondifferenceintersection (x::BI) y =  let z = x `union` y in 
	z == union (difference z y) (intersection z y)

prop_subsetGen (x :: BI) = (`isSubsetOf` x) `fmap` subsetGen x 

main = do
	print "Bag tests............"
	quickCheck (arbitrary :: Gen BI)
	quickCheck prop_unionSL
	quickCheck prop_intersectionSL
	quickCheck prop_differenceSL
	quickCheck prop_difference2isempty
	quickCheck prop_insertionSL
	quickCheck prop_uniondifferenceintersection
 	quickCheck prop_subsetGen





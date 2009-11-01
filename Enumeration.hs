{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies , MultiParamTypeClasses #-}

module Enumeration (
	CG (..),
	Result (..),
	Board,
	Node,
	solutions
	)
	where

import Data.List (delete,partition,group,sort)
import Data.Either (rights)
import Control.Monad (forever)
import Control.Monad.State (evalState, State, put, get)
import Control.Monad.Writer (runWriterT, WriterT, tell)


class (Eq g, Eq c, Ord g, Ord c) => CG g c | g -> c where
	attach :: g -> c -> [c] -> [Result (g,[c])]
	exit :: Board g -> Bool

type Board g = [Either g g]
type Node g c = ([c],Board g)

data Result a = R a | N a | E a 

core :: (CG g c, Eq c) => Node g c -> [Node g c]
core (h,b) = do 
	c <- h
	g <- rights b
	r <- attach g c (delete c h)
	return $ case r of 
		R (g',h') -> (h', Right g' : delete (Right g) b)
		N (g',h') -> (h', Right g' : b)
		E (g',h') -> (h', Left g' : delete (Right g) b)

line :: CG g c => [Node g c] -> ([Node g c],[Node g c])
line =  partition (exit . snd) . nubOrd . concatMap core where 
	nubOrd = map head . group . sort

sviluppo :: CG g c => WriterT [Node g c] (State [Node g c]) ()
sviluppo =  do
	(stay,exit) <- line `fmap` get
	put stay
	tell exit

solutions :: CG g c => Node g c -> [Node g c]
solutions x = snd . flip evalState [x] . runWriterT $ forever sviluppo 
		



{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies , MultiParamTypeClasses #-}

module Enumeration (
	CG (..),
	Result (..),
	Board,
	Node,
	solutions,
	)
	where

import Data.List (delete,partition,group,sort)
import Data.Either (partitionEithers)
import Control.Monad (forever)
import Control.Monad.State (evalState, State, put, get)
import Control.Monad.Writer (runWriterT, WriterT, tell)
import Control.Arrow ((***), second)
import Debug.Trace

class (Show g, Show c, Eq g, Eq c, Ord g, Ord c) => CG g c | g -> c where
	attach :: g -> c -> [c] -> [Result (g,[c])]

type Board g = [g]
type Node g c = ([c],Board g)

data Result a = R a | N a | E a 

core :: (CG g c, Eq c) => Node g c -> [Either (Node g c) (Node g c)]
core (h,b) = do 
	c <- h
	g <- b
	r <- attach g c (delete c h)
	return $ case r of 
		R (g',h') -> Right (h', g' : delete g b)
		N (g',h') -> Right (h', g' : b)
		E (g',h') -> Left (h', g' : delete g b)

line :: CG g c => [Node g c] -> ([Node g c],[Node g c])
line  = (dropper *** dropper) .  partitionEithers . concatMap core
	where	dropper = map head . group . sort  . map (second sort)

sviluppo :: CG g c => WriterT [Node g c] (State [Node g c]) ()
sviluppo =  do
	(exit,stay) <- line `fmap` get
	trace ("*****\n") $ put stay
	tell exit

solutions :: CG g c => Node g c -> [Node g c]
solutions x = snd . flip evalState [x] . runWriterT $ k where
	k = do
		sviluppo
		x <- get
		if null x then return () else k
		




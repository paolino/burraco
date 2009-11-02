{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies , MultiParamTypeClasses #-}

module Enumeration 
	{-
	(
	CG (..),
	Result (..),
	Board,
	GState (..),
	run,
	sviluppo,
	Node,
	solutions,
	)
	-}
	where

import Data.List (delete,partition,group,sort)
import Data.Either (partitionEithers)
import Control.Monad (forever)
import Control.Monad.State (runState, State, put, get)
import Control.Monad.Writer (runWriterT, WriterT, tell)
import Control.Arrow ((***), second, first)
import Debug.Trace

class (Eq g, Eq c, Ord g, Ord c) => CG g c v | g -> c, g -> v where
	attach :: GState g v -> c -> [c] -> [Result ([c], GState g v)]

data GState g v = GState {env::v , game::g}

instance Eq g => Eq (GState g v) where
	(GState _ g ) == (GState _ g') = g == g'

instance Ord g => Ord (GState g v) where
	(GState _ g ) `compare` (GState _ g') = g `compare` g'

type Board g v = [GState g v]
type Node g c v = ([c],Board g v)

data Result a = R a | N a | E a 

core :: CG g c v => Node g c v -> [Either (Node g c v) (Node g c v)]
core (h,b) = do 
	c <- h
	g <- b
	r <- attach g c (delete c h)
	return $ case r of 
		R (h',g') -> Right (h', g' : delete g b)
		N (h',g') -> Right (h', g' : b)
		E (h',g') -> Left (h', g' : delete g b)

line :: CG g c v => [Node g c v] -> ([Node g c v],[Node g c v])
line  = (dropper *** dropper) .  partitionEithers . concatMap core
	where	dropper = map head . group . sort  . map (second sort)

sviluppo :: CG g c v => WriterT [Node g c v] (State [Node g c v]) ()
sviluppo =  do
	(exit,stay) <- line `fmap` get
	put stay
	tell exit

run :: CG g c v =>  WriterT [Node g c v] (State [Node g c v]) () -> Node g c v -> ([Node g c v], [Node g c v])
run k x =  first snd . flip runState [x] . runWriterT $ k

tillend = do
	sviluppo
	x <- get
	if null x then return () else tillend
solutions :: CG g c v => Node g c v -> [Node g c v]
solutions x = fst $ run tillend x where
		




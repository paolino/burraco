
{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module Space where

import Control.Arrow (first, second, (&&&))
import Data.List (delete, group, sort)
import Data.Function (on)
import Control.Monad (liftM2)
import Data.Either (lefts)
import Data.Char (chr)
import Control.Parallel.Strategies
import Debug.Trace

import Card


data Combination = Come | Quando | Fuori | Piove | 
	Asso | Due | Tre | Quattro | Cinque | Sei  | Sette | Otto | Nove 
	| Dieci | Jack | Donna | Re deriving Enum

check :: Hand -> Combination -> Maybe (Game , Hand)
check h Come = let
	cs <- filter (suited 0) h


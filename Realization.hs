module Realization 
	where
import Control.Arrow (second, (&&&))
import Data.List ((\\),sortBy, groupBy , sort, partition, group)
import Data.Ord (comparing)
import Card (Card (..), ranked, suited, Rank, Suite)
import Data.Function (on)
import Control.Monad (ap)
import Burraco

realizza :: Combination -> 


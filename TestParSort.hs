import Control.Parallel.Strategies
import Control.Parallel
import Data.List
import System.Random
import System.Environment

parSort :: (NFData a, Ord a) => Int -> [a] -> [a]
parSort d list@(x:xs)
  | d <= 0    = sort list
  | otherwise = rnf (below,above) `seq` (rnf lesser `par` rnf greater) `seq` (lesser ++ x:greater)
      where (below,above) = partition (<x) xs
            lesser      = parSort d' below
            greater     = parSort d' above
            d' = d - 1
parSort _ _              = []


main = do
	r:n:_ <- getArgs
	xs <- take (read r) `fmap` randomRs (0,100000000000 :: Integer) `fmap` getStdGen
	
	print . last . parSort (read n) $ xs 

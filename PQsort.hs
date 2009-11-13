{-# LANGUAGE BangPatterns#-}
module PQsort where
import Control.Parallel


pqsort :: Ord a => [a] -> [a]
pqsort xs = pqsort' (length xs) xs

threshhold :: Int
threshhold = 2^6

pqsort' :: Ord a => Int -> [a] -> [a]
pqsort' len xs      | len < threshhold = qsort xs
pqsort'  _ (x:xs)   = ls' `par` hs' `par` ls' ++ (eqs ++ hs')
        where (ls, llen, eqs, hs, hlen) = part x xs ([],0,[x],[],0)
              ls' = pqsort' llen ls
              hs' = pqsort' hlen hs
pqsort' _ [] = []

qsort (x:xs) = qsort ls ++ (eqs ++ qsort hs)
    where (ls, eqs, hs) = part' x xs ([],[x],[])
qsort [] = []

          


part z (y:ys) (ls, !llen, eq, hs, !hlen) =
    case compare y z of
        LT -> part z ys (y:ls, llen+1, eq, hs, hlen)
        GT -> part z ys (ls, llen, eq, y:hs, hlen+1)
        _  -> part z ys (ls, llen, y:eq, hs, hlen)
part _ [] acc = acc
 
part' z (y:ys) (ls, eq, hs) =
  case compare y z of
      LT -> part' z ys (y:ls,eq,hs)
      GT -> part' z ys (ls,eq,y:hs)
      _ -> part' z ys (ls,y:eq,hs)
part' _ [] acc = acc


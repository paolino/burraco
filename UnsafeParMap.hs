module UnsafeParMap (unsafeParMap) where

import Control.Concurrent.STM
import Control.Concurrent
import System.IO.Unsafe

unsafeParMap n f xs = unsafeIOPerform $ do
	atomically $ newTChan c
	atomically $ newTChan d
	
	forkIO $ mapM_ (writeTChan c . Just) xs >> mapM_ (writeTChan c) Nothing
	replicateM n . forkIO $ let 
		k = do 
			r <- atomically $ do
				y <- readTChan c
				case y of
					Just x -> do 
						writeTChan d $ let z = f x in rnf z `seq` z 
						return True
					Nothing -> return False
			when r k
		in k
	 
			
		




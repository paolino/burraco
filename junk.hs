------------------  Test --------------------
instance Arbitrary Gioco where
	arbitrary = do
		t <- elements [False,True]
		case t of 
			True -> do 
				c1 <- elements [1..14]
				c2 <- elements [c1.. 14]
				s <- elements [0..3] 
				return . Scala . map (flip mkCard s) $ [c1 .. c2] 
			False -> do
				s <- elements $ tail $ powerset [0,0,1,1,2,2,3,3]
				r <- elements $ [1] ++ [3..13]
				return . Tris . map (mkCard r) $ s

data Gioco = Scala {unGioco :: [Card]} | Tris {unGioco :: [Card]} deriving Show

------------------------------------------------------


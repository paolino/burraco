
{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module Card where

import Control.Arrow (first, second, (&&&))
import Data.List (delete, group, sort)
import Data.Function (on)
import Control.Monad (liftM2)
import Data.Either (lefts)
import Data.Char (chr)
import Control.Parallel.Strategies
import Debug.Trace
import Control.Monad.State
import System.Random


-- | Valore di una carta di un qualunque seme. 
newtype Rank = Rank Int deriving (Read,Ord,Eq,Num, Enum,NFData)

instance Show Rank where
	show (Rank x) 
		| x == 1 = "A"
		| x == 13 = "K"
		| x == 12 = "Q"
		| x == 11 = "J"
		| x == 14 = "A"
		| otherwise = show $ x
-- | Seme di una carta.
newtype Suite = Suite Int deriving (Read,Ord,Eq,Num,NFData, Enum)

instance Show Suite where
	show (Suite y) 
		| y == 0 = "C"
		| y == 1 = "Q"
		| y == 2 = "F"
		| y == 3 = "P"

-- | Una carta contiene il suo valore ed il suo seme.I  jolly sono definiti con valore -20 sia per il seme che per il valore
newtype Card = Card {unCard :: (Rank, Suite)} deriving (Read,Eq, Ord,NFData)



instance Show Card where
	show (Card (x,y)) 
		| x == -20 = "Jolly"
		| otherwise = show x ++ show y



-- | Nel burraco una combinazione di carte al tavolo puo contenere solo una matta. Siccome le matte comprendono le pinelle e le pinelle sono anche carte normali, e' necessario ricordarsi se si trovano in una scala del loro stesso seme. PSA tiene conto delle matte nelle scale
data PSA 	
	-- | La scala contiene la matta all'interno della scala. Il suo valore e' definito
	= Piazzato 
		Rank	-- ^ che valore all'interno della scala occupa la matta  
		Bool  	-- ^ se la matta ha lo stesso seme della scala
	-- | La scala ha una matta all'esterno . Il suo valore e' potenziale tra sotto la minima e sopra la massima
	| Spiazzato 
		Bool 	-- ^ se la matta ha lo stesso seme della scala
	-- | La scala non ha matte associate
	| Assente deriving (Show,Eq,Ord,Read)
-- | Una combinazione di carte valide per stare sul tavolo del burraco
data Combination 
	-- | Un insieme di carte dello stesso valore
	= Tris 
		Rank 	-- ^ Il valore comune alle carte del tris
		Bool	-- ^ Indica se il tris contiene una matta
	-- | Un insieme di carte in sequenza e dello stesso seme
	| Scala 
		Suite 	-- ^ Il seme comune alle carte della sequenza
		PSA 	-- ^ Il tipo di piazzamento della eventuale matta
		(Rank,Rank)	-- ^ Il valore della prima carta e dell'ultima della sequenza
	-- | Il tavolo, inteso come la combinazione virtuale alla quale si possono attaccare le combinazioni di partenza
	| Tavolo
	-- | Il monte degli scarti, inteso come la combinazione virtuale che accetta una carta qualsiasi
	| Scarto deriving (Eq,Ord,Show,Read)

instance NFData Combination where
	rnf (Tris r j) = (rnf r) `seq` (rnf j) `seq` ()
	rnf (Scala s p d) = (rnf s) `seq` (rnf p) `seq` (rnf d) `seq` ()
	rnf _ = ()

instance NFData PSA where
	rnf (Piazzato r c) = rnf r `seq` rnf c `seq` ()
	rnf (Spiazzato t) = rnf t `seq` ()
	rnf _ = ()
-- | una carta ce rappresenta il jolly
jolly :: Card
jolly = Card (-20,-20)

-- | controlla che il valore di una carta sia pari al dato
ranked :: Rank -> Card -> Bool
ranked n = (==) n . fst . unCard

-- | controll che il seme di una carta sia quello dato
suited :: Suite -> Card -> Bool
suited s = (==) s . snd . unCard

-- | controlla se una carta e' un jolly o una pinella
isJP :: Card -> Bool
isJP = liftM2 (||) (ranked 2) (ranked (-20))

deck :: [Card]
deck = concat . replicate 2 $ [Card (i,Suite j) | i <- [1..13] , j <- [0..3]] ++ replicate 4 jolly

pickT = do
	rs <- get
	n <- (`mod` length rs) `fmap` lift randomIO
	put $ take n rs ++ drop (n + 1) rs
	return $ rs !! n

handT n  = replicateM n pickT

runT f = evalStateT f deck


{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}
module Burraco where

import Control.Arrow (first)
import Data.List (delete, groupBy, sortBy, find)
import Data.Function (on)
import Control.Monad (liftM2)
import Data.Either (lefts)
import Data.Char (chr)
import Enumeration
import Debug.Trace


-- | Valore di una carta di un qualunque seme. 
newtype Rank = Rank Int deriving (Read,Ord,Eq,Num, Enum)
instance Show Rank where
	show (Rank x) 
		| x == 1 = "A"
		| x == 13 = "K"
		| x == 12 = "Q"
		| x == 11 = "J"
		| x == 14 = "A"
		| otherwise = show $ x
-- | Seme di una carta.
newtype Suite = Suite Int deriving (Read,Ord,Eq,Num)

instance Show Suite where
	show (Suite y) 
		| y == 0 = "C"
		| y == 1 = "Q"
		| y == 2 = "F"
		| y == 3 = "P"

-- | Una carta contiene il suo valore ed il suo seme.I  jolly sono definiti con valore -20 sia per il seme che per il valore
newtype Card = Card {unCard :: (Rank, Suite)} deriving (Read,Eq, Ord)

instance Show Card where
	show (Card (x,y)) 
		| x == -20 = "Jolly"
		| otherwise = show x ++ show y

-- | Insieme di carte, detta anche mano
type Hand = [Card]

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

-- | valore di uscita per il metodo attach quando la combinazione viene modificata
subst :: Hand -> Combination -> [(Result (Combination, Hand))]
subst h x = return . R $ (x,h)

-- | valore di uscita per il metodo di attach, quando non ci sono vie possibili
nochance :: [(Result (Combination, Hand))]
nochance = []

-- | l'istanza a GC ci permette di sviluppare le mosse attraverso Enumeration.solutions
instance CG Combination Card [Card] where
	-- attaccare una carta al monte degli scarti, il valore di uscita e' un E segnalando la fine del ramo
	attach (GState cs Scarto) c h = [E (GState (c:cs) Scarto ,h)]
	-- attaccare a un tris, scala o tavolo, tutti qui sotto per gestire la ambivalenza dell'asso
	attach g (Card (r,s))  h = attach' g  (Card (r,s)) h ++ case r of
		1 -> attach' g (Card (14,s))  h -- i nodi seguenti all'interpretazione dell'asso come carta oltre il Re
		_ -> nochance

		where
		-- attaccare al tavolo ovvero calare una combinazione nuova (codice difficile :P )
		attach' (GState Tavolo []) c@(Card (r, s)) h 
			| isJP c = nochance -- evitiamo di tentare una calata partendo da una matta, tanto il caso e' sussunto sotto 
				-- e i tris di pinelle vietati da regolamento
			| otherwise = let 
				-- interpreta il confronto con 1 come confronto con 14
				rankedWA n = ranked $ case n of 
					14 -> 1
					x -> x
				block k = map (N . first k . head) . groupBy ((==) `on` snd) . sortBy (compare `on` snd)
				specPicks k r1 r2 = block k . picks r1 (checks s [ranked r1, rankedWA r2]) isJP $ h

				t = let 	k ((Just _),cs) = GState (c:cs) $ Tris r True 
						k (_,cs) =  GState (c:cs) $ Tris r False 
						in block k . picks 0 (replicate 2 $ ranked r) isJP $  h
				s0 r	| r > 2 = let 	
						k (Just (n,x),cs) = let 
							t = suited s x
							in GState (c:cs) . uncurry (Scala s) $ case n == r - 2 of
								True -> (Spiazzato t, (r - 1, r))
								_  -> (Piazzato n t, (r - 2, r))
						k (_,cs) = GState (c:cs) $ Scala s Assente (r - 2,r) 
						in specPicks k (r - 2) (r - 1) 
					| otherwise = nochance
				s1 r 	| (r >  1 && r <  14) = let
						k (Just (n,_),cs) = let
							p = case n == r -  1 of
								True -> (r , r +  1)
								_ -> (r -  1, r)
							in GState (c:cs) $ Scala s (Spiazzato $ suited s c) p 
						k (_,cs) = GState (c:cs) $ Scala s Assente (r -  1,r +  1) 
						in specPicks k (r - 1) (r + 1)
					| otherwise = nochance
				s2 r 	| r <  13 = let
						k (Just (n,x),cs) = let 
							t = suited s x
							in GState (c:cs) . uncurry (Scala s) $ case n == r +  2 of
								True  -> (Spiazzato t, (r, r +  1))
								_ -> (Piazzato n t, (r, r +  2))
						k (_,cs) = GState (c:cs) $ Scala s Assente (r, r + 2) 
						in specPicks k (r + 1) (r + 2) 
					| otherwise = nochance
				in concat [t,s0 r ,s1 r ,s2 r] 
		attach' (GState cs g) c h = case attach'' g c of
			Nothing -> nochance
			Just g' -> return . R $ (GState (c:cs) g',h)
			where
			-- attaccare ad un tris
			attach'' t@(Tris ra j cs) (Card (r, _)) h
				| r == ra = Just $ Tris ra j -- la carta a il valore giusto per entrare
				| (r == 2 || r == -20) && not j = Just $ Tris ra True  -- la carta e' un jolly 
				| oterwise = Noting

			-- attaccare ad una scala con jolly o pinella all'interno
			attach'' (Scala s (Piazzato r t) (r0,r1)) (Card (r', s'))  
				| s /= s' = Noting -- seme incompatibile (la matta e' giá presente)
				| r == r' && t && r0 == 3 = Just $ Scala s Assente (2,r1)  -- la pinella va a finire nel 2  
				| r == r' = Just $ Scala s (Spiazzato t) (r0,r1)   -- la pinella a lo stesso seme ma non finisce nel 2
				| r' == r0 - 1 = Just $ Scala s (Piazzato r t) (r',r1)   -- la carta attacca sotto
				| r' == r1 + 1 = Just $ Scala s (Piazzato r t) (r0,  r')   -- la carta attacca sopra
				| oterwise = Noting -- valore lontano dalla scala

			-- attaccare ad una scala con jolly o pinella all'esterno
			attach'' (Scala s (Spiazzato t) (r0,r1) ) (Card (r', s'))  
				| s /= s' = Noting -- seme incompatibile (la matta e' giá presente)
				| r' == r0 - 1 = Just $ Scala s (Spiazzato t) (r',r1)   -- la carta attacca sotto senza la matta
				| r' == r0 - 2 = Just $ Scala s (Piazzato (r0 - 1) t) (r',r1)   -- la carta attacca sotto con la matta interposta
				| r' == r1 + 1 = Just $ Scala s (Spiazzato t) (r0, r')   -- la carta attacca sopra senza la matta
				| r' == r1 + 2 = Just $ Scala s (Piazzato (r0 + 1) t) (r', r1)   -- la carta attacca sopra con la matta interposta
				| oterwise = Noting -- valore lontano dalla scala

			-- attaccare ad una scala ce non contiene jolly o pinelle
			attach'' (Scala s Assente (r0,r1) )  (Card (r', s')) 
				| s /= s' && isJP c = Just $ Scala s (Spiazzato False) (r0,r1)   -- jolly o pinella accettato
				| s /= s' = Noting -- seme incompatibile per una non pinella
				| r' == r0 - 1 = Just $ Scala s Assente (r',r1)    -- la carta attacca sotto
				| r' == r1 + 1 = Just $ Scala s Assente (r0, r')   -- la carta attacca sopra
				| oterwise = Noting -- valore lontano dalla scala


-- aggiunge la appartenenza ad un seme a una lista di condizioni
checks :: Suite -> [Card -> Bool] -> [Card -> Bool]
checks s = map $ liftM2 (&&) (suited s)

-- | Un tipo che ci permette di segnalare e tracciare l'utilizzo di un Jolly nella raccolta di un elemento da una lista
type Jolly a = Either 
	(a -> Bool) -- la funzione che selezione un elemento alternativo a qualsiasi altro , aka jolly
	(Rank, a) -- la posizione dell'elemento alternativo piazzato

-- | pesca tutti gli elementi a turno da una lista che soddisfa una condizione o un elemetno atlernativo se ancora disponibile. Gli argomenti sono raggruppati per rendere la funzione compatibile con foldr
pick :: Eq a 
	=> ((a -> Bool), Rank) -- ^ (condizione da soddisfare per un elemento, marcatore di posizione)
	-> (Jolly a,[a],[a]) -- ^ (stato di jolly, lista rimanente,lista creata)
	-> [(Jolly a,[a],[a])] -- ^ possibili stati successivi
pick (k,i) (j,cs,xs) = concat [
		[(j, delete c cs, c:xs) | c <- filter k cs],
		case j of 
			Left k' -> [(Right (i, c), delete c cs, c:xs) | c <- filter k' cs]
			_ -> []
		]

-- | calcola le soluzioni di pescata da una lista per una lista di condizioni
picks :: Eq a => Rank -> [a -> Bool] -> (a -> Bool) -> [a] -> [((Maybe (Rank,a),[a]),[a])]
picks i ks j cs = map q . foldr (concatMap . pick) [(Left j,cs,[])] $ zip ks [i..]
	where 	q ((Left _),cs,xs) = ((Nothing,xs),cs)
		q ((Right x),cs,xs) = ((Just x,xs),cs)





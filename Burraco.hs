{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module Burraco where

import Control.Arrow (first, second, (&&&))
import Data.List (delete, group, sort)
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


-- | valore di uscita per il metodo di attach, quando non ci sono vie possibili
nochance :: [Result (Hand, GState Combination [Card])]
nochance = []

-- | l'istanza a CG ci permette di sviluppare le mosse attraverso Enumeration.solutions, come environment delle combinazioni , mettiamo le carte che contengono. Da notare che questa informazione non ha alcun valore per lo sviluppo delle mosse, ma potrebbe servire a comunicare e a valutare lo stato del gioco. La lista di carte contenute non e' inserita direttamente nei giochi anche per non appesantire il confronto tra i nodi nella ricerca e semplificazione di simmetrie.
instance CG Combination Card [Card] where
	-- attaccare una carta al monte degli scarti, il valore di uscita e' un E segnalando la fine del ramo
	attach (GState cs Scarto) c h = [E (h, GState (c:cs) Scarto )]
	-- attaccare a un tris, scala o tavolo, tutti qui sotto per gestire la ambivalenza dell'asso
	attach g (Card (r,s))  h = attach' g  (Card (r,s)) h ++ case r of
		1 -> attach' g (Card (14,s))  h -- i nodi seguenti all'interpretazione dell'asso come carta oltre il Re
		_ -> nochance

		where
		-- attaccare al tavolo ovvero calare una combinazione nuova (codice difficile :P )
		attach' (GState [] Tavolo) c@(Card (r, s)) h 
			| isJP c = nochance -- evitiamo di tentare una calata partendo da una matta, tanto il caso e' sussunto sotto 
				-- e i tris di pinelle vietati da regolamento
			| otherwise = let 
				-- qui facciamo il numero, creiamo un tavolo virtuale contentente o un tris di una carta o una scala
				-- e lanciamo esattamente due passaggi di sviluppo sul tavolo e sulle carte restanti.
				-- ovviamente verranno computate a turno solo i casi sottostanti (tris e scala)
				-- dopo due turni avremo creato tutti i tris e le scale da tre che si possono costruire intorno alla
				-- carta data. Quindi ce ne usciamo con degli N che infatti sono da inserire nel tavolo di partenza
				-- Da notare che non essendoci le condizioni di uscita delle strade (Result in E) che avvengono solo per gli
				-- scarti, ci tocca intercettare i nodi incompiuti (snd . run) infatti quelli compiuti sono vuoti (fst . run)
				-- Qui e' l'apoteosi della lazyness, infatti attach e sviluppo sono mutualmente ricorsive (vedi Enumeration.core)
				-- inoltre solo con le typeclass si evita il loop tra i moduli
				runtwo b =  map (N . second head) . snd . run (sviluppo >> sviluppo) $ (h,[b])
				in concatMap (runtwo . GState [c]) [Tris r False, Scala s Assente (r,r)]-- ,s0 r ,s1 r ,s2 r] 
		-- i casi restanti sono tutti potenziali rimpiazzamenti di un gioco esistente, li wrappiamo per semplificarne la scrittura
		attach' (GState cs g) c h = case attach'' g c of
			Nothing -> nochance
			Just g' -> return . R $ (h, GState (c:cs) g')
			where
			-- attaccare ad un tris
			attach'' t@(Tris ra j) c@(Card (r, _)) 
				| r == 14 = Nothing -- eh, di tris di assi ce ne e' solo uno 
				| r == ra = Just $ Tris ra j -- la carta a il valore giusto per entrare
				| (r == 2 || r == -20) && not j = Just $ Tris ra True  -- la carta e' un jolly 
				| otherwise = Nothing

			-- attaccare ad una scala con jolly o pinella all'interno
			attach'' (Scala s (Piazzato r t) (r0,r1)) (Card (r', s'))  
				| s /= s' = Nothing -- seme incompatibile (la matta e' giá presente)
				| r == r' && t && r0 == 3 = Just $ Scala s Assente (2, r1)  -- la pinella va a finire nel 2  
				| r == r' = Just $ Scala s (Spiazzato t) (r0, r1)   -- la pinella a lo stesso seme ma non finisce nel 2
				| r' == r0 - 1 = Just $ Scala s (Piazzato r t) (r', r1)   -- la carta attacca sotto
				| r' == r1 + 1 = Just $ Scala s (Piazzato r t) (r0, r')   -- la carta attacca sopra
				| otherwise = Nothing -- valore lontano dalla scala

			-- attaccare ad una scala con jolly o pinella all'esterno
			attach'' (Scala s (Spiazzato t) (r0,r1) ) (Card (r', s'))  
				| s /= s' = Nothing -- seme incompatibile (la matta e' giá presente)
				| r' == r0 - 1 = Just $ Scala s (Spiazzato t) (r',r1)   -- la carta attacca sotto senza la matta
				| r' == r0 - 2 = Just $ Scala s (Piazzato (r0 - 1) t) (r',r1)   -- la carta attacca sotto con la matta interposta
				| r' == r1 + 1 = Just $ Scala s (Spiazzato t) (r0, r')   -- la carta attacca sopra senza la matta
				| r' == r1 + 2 = Just $ Scala s (Piazzato (r1 + 1) t) (r0, r')   -- la carta attacca sopra con la matta interposta
				| otherwise = Nothing -- valore lontano dalla scala

			-- attaccare ad una scala ce non contiene jolly o pinelle
			attach'' (Scala s Assente (r0,r1) )  (Card (r', s')) 
				| s /= s' && isJP c = Just $ Scala s (Spiazzato False) (r0,r1)   -- jolly o pinella accettato
				| s /= s' = Nothing -- seme incompatibile per una non pinella
				| r' == r0 - 1 = Just $ Scala s Assente (r',r1)    -- la carta attacca sotto
				| r' == r1 + 1 = Just $ Scala s Assente (r0, r')   -- la carta attacca sopra
				| otherwise = Nothing -- valore lontano dalla scala




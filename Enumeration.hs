{-# LANGUAGE NoMonomorphismRestriction, FunctionalDependencies , MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}


import Data.List (delete,partition,group,sort,cycle,tails)
import Data.Either (partitionEithers)
import Control.Monad (forever)
import Control.Monad.State (runState, State, put, get)
import Control.Monad.Writer (runWriterT, WriterT, tell)
import Control.Arrow ((***), second, first)
import Control.Parallel.Strategies 
import Control.Parallel 
import Debug.Trace
import Control.Arrow (first, second, (&&&))
import Data.List (delete, group, sort)
import Data.Function (on)
import Control.Monad (liftM2)
import Data.Either (lefts)
import Data.Char (chr)
import Control.Parallel.Strategies
import Card
import Debug.Trace
import Control.Monad.State
import System.Environment
import System.Random


---------------------------
data GState g v = GState {env::v , game::g}

instance NFData g => NFData (GState g v) where
	rnf (GState v g) = (rnf g) `seq` ()

instance Eq g => Eq (GState g v) where
	(GState _ g ) == (GState _ g') = g == g'

instance Ord g => Ord (GState g v) where
	(GState _ g ) `compare` (GState _ g') = g `compare` g'

-------------------------------


class Consumabile h  where
	subStates :: h -> [h]

data Result a = R a | N a | E a 

class (Eq g, Eq h, Ord g, Ord h, NFData g, NFData h, Consumabile h) => CG g h v | g -> v, g -> h where
	attach :: GState g v -> h -> [Result (h, GState g v)]
-------------------------------------------

type Board g v = [GState g v]
type Node g h v = (h,Board g v)


core :: CG g h v => Node g h v -> [Either (Node g h v) (Node g h v)]
core (h,b) = do 
	sh <- subStates h
	g <- b
	r <- attach g sh
	return $ case r of 
		R (h',g') -> Right (h', g' : delete g b)
		N (h',g') -> Right (h', g' : b)
		E (h',g') -> Left (h', g' : delete g b)

line :: CG g h v => [Node g h v] -> ([Node g h v],[Node g h v])
line  = (dropper *** dropper) .  partitionEithers . parFlatMap rnf core
	where	dropper = map head . group . parSort . map (second sort)

sviluppo :: CG g h v => WriterT [Node g h v] (State [Node g h v]) ()
sviluppo =  do
	(exit,stay) <- line `fmap` get
	put stay
	tell exit

run :: CG g h v =>  WriterT [Node g h v] (State [Node g h v]) () -> Node g h v -> ([Node g h v], [Node g h v])
run k x =  first snd . flip runState [x] . runWriterT $ k

tillend = do
	sviluppo
	x <- get
	if null x then return () else tillend

solutions :: CG g h v => Node g h v -> [Node g h v]
solutions  = fst . run tillend  
		

parSort :: (NFData a, Ord a) => [a] -> [a]
parSort list@(x:xs)
  | length list < 100    = sort list
  | otherwise =  rnf q `pseq` (rnf lesser `par` rnf greater) `pseq` (lesser ++ x:greater)
      where q@(below,above) = partition (<  x) xs
            lesser      = parSort  below
            greater     = parSort  above
parSort  _ = []


------------------------ burraco specific -----------------------------------------
instance Consumabile [Card] where
	subStates [] = []
	subStates xs = do 
		ys <- take l . tails $ cycle xs
		return $ take l ys
		where l = length xs 
	
-- | valore di uscita per il metodo di attach, quando non ci sono vie possibili
nochance :: [Result ([Card], GState Combination [Card])]
nochance = []

-- | l'istanza a CG ci permette di sviluppare le mosse attraverso Enumeration.solutions, come environment delle combinazioni , mettiamo le carte che contengono. Da notare che questa informazione non ha alcun valore per lo sviluppo delle mosse, ma potrebbe servire a comunicare e a valutare lo stato del gioco. La lista di carte contenute non e' inserita direttamente nei giochi anche per non appesantire il confronto tra i nodi nella ricerca e semplificazione di simmetrie.
instance CG Combination [Card] [Card] where
	attach _ [] = error "could not attach no card" 
	-- attaccare una carta al monte degli scarti, il valore di uscita e' un E segnalando la fine del ramo
	attach (GState cs Scarto) (c:h) = [E (h, GState (c:cs) Scarto )]
	-- attaccare a un tris, scala o tavolo, tutti qui sotto per gestire la ambivalenza dell'asso
	attach g (Card (r,s):h) = attach' g  (Card (r,s)) h ++ case r of
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





----------------- tests -------------------------------

deck :: [Card]
deck = concat . replicate 2 $ [Card (i,Suite j) | i <- [1..13] , j <- [0..3]] ++ replicate 4 jolly

pickT = do
	rs <- get
	n <- (`mod` length rs) `fmap` lift randomIO
	put $ take n rs ++ drop (n + 1) rs
	return $ rs !! n

handT n  = replicateM n pickT

runT f = evalStateT f deck
main = do
	r:_ <- getArgs
	h0 <- runT $ handT (read r) 
	print h0
	mapM_ print . map (second $ map env) $ solutions (h0,[GState [] Tavolo, GState [] Scarto ])
{-
main' = do
	h:_ <- getArgs
	let 	h0 = map (Card . (Rank *** Suite)) $ read h :: Hand
	mapM_ print . map (second $ map game) . snd . run (sviluppo) $ (h0,[GState [] Tavolo, GState [] Scarto ])
-}

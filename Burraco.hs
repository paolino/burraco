{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module Burraco where

--import Control.Arrow (first, second, (&&&))
--import Data.List (delete, group, sort)
--import Data.Function (on)
import Control.Monad (liftM2)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader, ask , local)
import Test.QuickCheck
--import Data.Either (lefts)
--import Data.Char (chr)
import Control.Parallel.Strategies (NFData)
import Card
import Game
import Control.Monad.State
--import Debug.Trace


	

data Contesto = Contesto {
	dotazione :: (Cards,(Cards,Int)), -- la propria dotazione accoppiata con la conoscenza parziale che l'avversario ha
	dotazioneAvversario :: (Cards,Int), -- la conoscenza parziale che abbiamo dell'avversario
	banco :: [Cards],	-- i giochi davanti a noi
	bancoAvversario :: [Cards], -- i giochi di fronte a noi
	monteScarti :: (Cards,Card),	-- il monte degli scarti con specificata la carta in testa
	pozzetto :: Bool, -- se abbiamo preso il pozzetto
	pozzettoAvversario :: Bool, -- se l'avversario ha preso il pozzetto
	tallone :: Int -- le carte rimaste nel tallone (questa e' deducibile, quindi un codice di controllo integrità)
	}

newtype Contestuale m a = Contestuale (ReaderT Contesto m a)  deriving (Monad, MonadReader Contesto,Functor)




-- regole di validita' per un contesto
-- 1) la somma del numero di carte in dotazione, dotazioneAvversario,banco,bancoAvversario,tallone e 11*pozzetto, 11*pozzettoAvversario deve essere 108
-- 2) l'unione delle carte contenute nei vari insiemi conosciuti deve essere un sottoinsieme del mazzo da 108
-- 3) i giochi dei due banchi devono essere validi
-- 4) il giocatore che non ha preso il  pozzetto deve avere almeno 11 carte tra la sua dotazione ed il suo banco
-- 5) il giocatore che ha preso il pozzetto deve avere almeno 22 carte tra la sua dotazione ed il suo banco delle quali 11 devono essere nel banco
-- 6) se un giocatore ha dotazione nulla, almeno 22 carte devono essere nel duo banco e la testa del monte degli scarti non deve essere una matta e l'avversario non puo' avere dotazione nulla
-- inoltre data la aggiunta delle conoscenze parziali
-- 7) la conoscenza parziale che l'avversario ha di noi deve essere un sott'insieme della nostra dotazione

prop_somma (Contesto (d,_) (pda,lupda) b ba (ms,_) p pa t) = 108 == sum [
	size d, size pda, lupda, size b, size ba, size ms, 
	t, if p then 0 else 11, if pa then 0 else 11]

prop_unione (Contesto (d,_) (pda,_) b ba (ms,_) _ _ _) = let i = unions [d,pda,b,ba] in intersection i (fromList deck) == i

prop_tris = 
valid g d = 
genGioco matta = do
	deck <- get
	
	suchThat arbitrary (size (intersection 
		

instance Arbitrary Contesto where
	arbirary = flip evalStateT (fromList deck) $ do
		po <- lift $ elements [False,True]
		poa <- lift $ elements [False,True]
		
		case
		-- banchi .. semplice
		-- monteScarti , fondamentalmente un ammasso di carte qualsiasi, non troppo grande , un po lontane dalle dotazioni, molto lontane dai banchi 
		-- tallone è il resto

	



newtype Value = Value Double deriving (Eq,Show,Num,NFData)
mkValue = Value . press
	where press x = (pi/2 + atan x)/pi

type Feature = [(Cards, Value)]



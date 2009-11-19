{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module Contesto where

import Control.Monad (sequence)
import Control.Monad.Reader (ReaderT, MonadReader, lift)
import Card (Card,deck)
import Game (Cards, giocoGen)
import Bag (size, isSubsetOf, intersection, unions, difference, subsetGen1, subsetGen, fromList, toList)
import Control.Monad.State (evalStateT, StateT, get, put)
import Test.QuickCheck (elements, suchThat, listOf1, Testable (..), Gen, Arbitrary (..) , sized, resize , quickCheck)

data Contesto = Contesto {
	dotazione :: (Cards,(Cards,Int)), -- la propria dotazione accoppiata con la conoscenza parziale che l'avversario ha
	dotazioneAvversario :: (Cards,Int), -- la conoscenza parziale che abbiamo dell'avversario
	banco :: [Cards],	-- i giochi davanti a noi
	bancoAvversario :: [Cards], -- i giochi di fronte a noi
	monteScarti :: (Cards,Card),	-- il monte degli scarti con specificata la carta in testa
	pozzetto :: Bool, -- se abbiamo preso il pozzetto
	pozzettoAvversario :: Bool, -- se l'avversario ha preso il pozzetto
	tallone :: Int -- le carte rimaste nel tallone (questa e' deducibile, quindi un codice di controllo integrità)
	} deriving (Show,Read)

newtype Contestuale m a = Contestuale (ReaderT Contesto m a)  deriving (Monad, MonadReader Contesto,Functor)

-- regole di validita' per un contesto
-- 1) la somma del numero di carte in dotazione, dotazioneAvversario,banco,bancoAvversario,tallone e 11*pozzetto, 11*pozzettoAvversario deve essere 108
-- 2) l'unione delle carte contenute nei vari insiemi conosciuti deve essere un sottoinsieme del mazzo da 108
-- 3) i giochi dei due banchi devono essere validi
-- 4) il giocatore che non ha preso il  pozzetto deve avere almeno 11 carte tra la sua dotazione ed il suo banco
-- 5) il giocatore che ha preso il pozzetto deve avere almeno 22 carte tra la sua dotazione ed il suo banco delle quali 11 devono essere nel banco
-- 6) se un giocatore ha dotazione nulla, almeno 22 carte devono essere nel suo banco e la testa del monte degli scarti non deve essere una matta e l'avversario non puo' avere dotazione nulla
-- inoltre data la aggiunta delle conoscenze parziali
-------------------------------------- proprieta' dei contesti -------------------
prop_somma (Contesto (d,_) (pda,lupda) b ba (ms,_) p pa t) = 108 ==  sum [
	size d, size pda, lupda, size (unions b), size (unions ba), size ms, t]

prop_unione (Contesto (d,_) (pda,_) b ba (ms,_) _ _ _) = let i = unions ([d] ++ [pda] ++ b ++ ba) in intersection i (fromList deck) == i

prop_dotazioneN (Contesto (d,_) _ b _ _ p _ _ ) = let 
	u = size $ unions b 
	s = u + size d
	in if not p then 
		s > 10
		else s > 21 && u > 10	
prop_dotazioneNa (Contesto _ (pda,n) _ ba _ _ pa _ ) = let 
	u = size $ unions ba 
	s = u + size pda + n
	in if not pa then 
		s > 10
		else s > 21 && u > 10	

instance Testable Contesto where
	property = property . and . sequence [prop_somma, prop_unione, prop_dotazioneN, prop_dotazioneNa]
--------------------------------------------------------------------------------
type WithDeck = StateT Cards Gen

picking :: (Cards -> Gen (a,Cards)) -> WithDeck a
picking f = do
	deck <- get
	(x,d) <- lift $ f deck
	put $ difference deck d
	return x

resize' f g =  sized $ \n -> resize (f n) g  

bancoGen :: Bool -> Int -> WithDeck [Cards]
bancoGen t l = picking $ \deck -> do
	bs <- resize' (floor . sqrt . fromIntegral) $ listOf1 (giocoGen `suchThat` ((>2) . size)) `suchThat` k deck
	return (bs, unions bs)	
	where k deck gs = 
		let 	ugs = unions gs 
			s = size ugs 
		in and [
			s > if t then 10 else 0,
			s < l,
			ugs `isSubsetOf` deck]
	
dotazioneGen :: Bool -> [Cards] -> Int -> WithDeck (Cards,Cards,Int)
dotazioneGen t b l = picking $ \deck -> do
	d <- subsetGen1 deck `suchThat` k 	
	pd <- subsetGen d
	return ((d,pd,size d - size pd),d)
	where k g = and [
		size g < l,
		if t then size g + size (unions b) > 21 
				else size g > 10
		]
scartiGen :: WithDeck (Cards,Card)
scartiGen = picking $ \deck -> do 
	ms <- subsetGen1 deck
	hms <- elements $ toList ms
	return ((ms,hms), ms)


instance Arbitrary Contesto where
	arbitrary = flip evalStateT (fromList deck) $ do
		po <- lift $ elements [False,True]
		poa <- lift $ elements [False,True]
		b <- bancoGen po 40 
		ba <- bancoGen poa 40
		(d,pd,pdi) <-  dotazioneGen po b 30
		(_,pda,pdai) <- dotazioneGen poa ba 30
		ms <- scartiGen
		t <- size `fmap` get
		return (Contesto (d,(pd,pdi)) (pda,pdai) b ba ms po poa t)		 

main = quickCheck (arbitrary :: Gen Contesto)




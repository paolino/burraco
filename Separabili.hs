module Separabili (separabili) 
	where

import Control.Arrow ((***),(&&&))
import Control.Parallel.Strategies (rnf,parMap)
import Control.Monad.Reader (asks)
import Bag (difference, intersection, empty, size)
import Game (giochi,Cards)
import Burraco (Contestuale, Contesto(dotazione), mkValue, Feature)

-- | fornisce un valore ai giochi in base alla loro separazione rispetto agli altri. feature > 0.5 
separabili :: Monad m => Contestuale m (Feature)
separabili = do 
	c <- asks dotazione
	return . parMap rnf  (id  *** mkValue . fromIntegral . sum . map size  ) $ intable c giochi

-- | tabella delle intersezioni per ogni gioco, la lista di intersezioni del gioco stesso con la dotazione privata di tutti i giochi, a turno. La tabella produce solo le righe dei giochi che hanno intersezione non nulla con la dotazione. Le intersezioni sono esguite in parallelo
intable 	:: Cards		-- ^ dotazione   
		-> [Cards] 		-- ^ giochi considerati
		-> [(Cards,[Cards])]	-- ^ associazione gioco e intersezioni del gioco con le dotazioni corte
intable xs ys = let 	
			ys' = filter (not . empty . snd) . map (id &&& intersection xs) $ ys -- seleziona giochi utili
			ws = map (difference xs) . map snd $ ys' -- mani decurtate
			f (g,is) = (g,map (intersection is) ws)
		in   parMap rnf f ys' -- intersezioni di tutti i giochi con le mani decurtate




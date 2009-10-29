import Control.Applicative (liftA2)
import Data.List (groupBy)
import Data.Function (on)

type Rank = Int
type Suite = Int

data Card = Card {
	rank :: Rank,
	suite :: Suite
	}	deriving Ord

data Hand = Hand [Card]

mkHand :: [Card] -> Hand
mkHand =  Hand . sort

type Game = [Card]

games :: Hand -> [Game]
games = liftA2 (++) scalaGames trisGames

rankGames :: Hand -> [Game]
rankGames = groupBy ((==) `on` rank)

scalaGames :: Hand -> [Game]
scalaGames = undefined

desuite :: Hand -> [Hand]
desuite = groupBy ((==) `on` suite) . sortBy (comparing `on` suite)


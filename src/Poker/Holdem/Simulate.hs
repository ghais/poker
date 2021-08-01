-- |
-- Module      : Poker.Holdem.Simulate
-- Description : Monte Carlo simulation of a poker holdem game.
-- Copyright   : (c) Ghais Issa, 2021
--
-- Uses Monte Carlo Methods to determine the probability for each player in a game winning.
-- The simulation randomize each of the unknown cards in the game, this could include any
-- number of players' cards as well as flop, turn and river.
module Poker.Holdem.Simulate
  (
    Player(..)
  , Game(..)
  , simulate
  ) where

import           Data.Random (RVar, RandomSource, runRVar)

import           Control.Monad.State
import           Data.List (transpose)

import           Data.Random.Source.DevRandom (DevRandom (DevRandom))
import           Poker.Deck
import qualified Poker.Deck as Deck (shuffle)
import           Poker.Holdem
import           Poker.Holdem.Evaluate (HandRank, evaluate)

-- | A player can have 0, 1, 2 known cards.
data Player = Player
  {
    card1 :: Maybe Card
  , card2 :: Maybe Card
  }

-- | An abstraction that represents a poker game, with some unknowns.
--This allows us to simulate a game from any possible state.
data Game = Game
  {
    players :: [Player]      -- ^ Players.
  , flop    :: Maybe Flop    -- ^ The flop if known. Nothing otherwise.
  , turn    :: Maybe Turn    -- ^ The turn if known. Nothing otherwise.
  , street  :: Maybe Street  -- ^ The street if known. Nothing otherwise.
  }

-- | Run a Monte Carlo simulation of a game returning the probability of winning for each player.
simulate :: (RandomSource m DevRandom) =>
     Int        -- ^ Number of trajectories.
  -> Game         -- ^ State of the game before simulation.
  -> m [Double] -- ^ Probability for each player winning the game.
simulate n game = do
  gameHands <- replicateM n $ simulateWinners game
  return $ map ((/fromIntegral n) . sum) (transpose gameHands)

dealtCards :: Game -> [Card]
dealtCards Game{..} = let playerCards = concatMap dealtHands players
                          dealtHands (Player Nothing Nothing)     = []
                          dealtHands (Player (Just c1) Nothing)   = [c1]
                          dealtHands (Player Nothing (Just c2))   = [c2]
                          dealtHands (Player (Just c1) (Just c2)) = [c1, c2]
                          flopCards = case flop of
                            (Just (Flop c1 c2 c3)) -> [c1, c2, c3]
                            _                      -> []
                          turnCard = case turn of
                            (Just (Turn c)) -> [c]
                            _               -> []
                          streetCard = case street of
                            (Just (Street c)) -> [c]
                            _                 -> []
                      in playerCards ++  flopCards ++ turnCard ++ streetCard

completeHands :: Game -> StateT Deck Maybe [[Card]]
completeHands Game{..} = do
  playersCards <- mapM getPlayersCards players
  (Community (Flop c3 c4 c5) (Turn c6) (Street c7)) <- getCommunityCards flop turn street
  return [[c1, c2, c3, c4, c5, c6, c7] | (Hole c1 c2) <- playersCards]

getPlayersCards :: Player -> StateT Deck Maybe Hole
getPlayersCards player = do
  deck <- get
  (cards, deck') <- lift $ draw1 (neededCards player) deck
  put deck'
  return (hole player cards)
  where neededCards (Player Nothing Nothing)   = 2
        neededCards (Player (Just _) Nothing)  = 1
        neededCards (Player Nothing (Just _))  = 1
        neededCards (Player (Just _) (Just _)) = 0
        hole (Player (Just c1) (Just c2)) []   = Hole  c1 c2
        hole (Player (Just c1) Nothing) [c2]   = Hole c1 c2
        hole (Player Nothing (Just c2)) [c1]   = Hole c1 c2
        hole (Player Nothing Nothing) [c1, c2] = Hole c1 c2
        hole _ _                               = undefined


getCommunityCards :: Maybe Flop -> Maybe Turn -> Maybe Street -> StateT Deck Maybe Community
getCommunityCards Nothing _ _ = do
  deck <- get
  ([c1, c2, c3, c4, c5], deck') <- lift $ draw1 5 deck
  put deck'
  return (Community (Flop c1 c2 c3) (Turn c4) (Street c5))

getCommunityCards (Just (Flop c1 c2 c3)) Nothing _ = do
  deck <- get
  ([c4, c5], deck') <- lift $ draw1 2 deck
  put deck'
  return (Community (Flop c1 c2 c3) (Turn c4) (Street c5))

getCommunityCards (Just (Flop c1 c2 c3)) (Just(Turn c4)) Nothing = do
  deck <- get
  ([c5], deck') <- lift $ draw1 1 deck
  put deck'
  return (Community (Flop c1 c2 c3) (Turn c4) (Street c5))
getCommunityCards (Just (Flop c1 c2 c3)) (Just(Turn c4)) (Just (Street c5)) = do
  return (Community (Flop c1 c2 c3) (Turn c4) (Street c5))



playerHands :: Game -> RVar [[Card]]
playerHands game = do
  deck <- Deck.shuffle (gameDeck game)
  case evalStateT (completeHands game) deck of
    (Just cards) -> return cards
    _            -> return []

gameDeck :: Game -> Deck
gameDeck game = remove (dealtCards game) stdDeck



winners :: [HandRank] -> [Bool]
winners scores = map (== minRank) scores where
  minRank = maximum scores

averageScore :: [Bool] -> [Double]
averageScore winnerList = map (\x -> if x then 1/fromIntegral numWinners else 0) winnerList where
  numWinners = length (filter (== True) winnerList)


simulateOne :: (RandomSource m DevRandom) => Game -> m [([Card], HandRank)]
simulateOne game = do
  cards <- runRVar (playerHands game) DevRandom
  let scores = map evaluate' cards
  return $ zip cards scores
  where evaluate' [c1, c2, c3, c4, c5, c6, c7] = evaluate c1 c2 c3 c4 c5 c6 c7
        evaluate' _                            = undefined

simulateWinners :: (RandomSource m DevRandom) => Game -> m [Double]
simulateWinners game = do
  scores <- map snd <$> simulateOne game
  let gameWinners = winners scores
  return $ averageScore gameWinners








-- | ...
-- Module      : Poker.Holdem
-- Description : Texas Hold'em module
-- Copyright   : (c) Ghais Issa 2021
--
-- A variant of poker where two cards, known as hole cards, are dealt face down to each player,
-- and then five community cards are dealt face up in three stages. The stages consist of a series
-- of three cards ("the flop"), later an additional single card ("the turn" or "fourth street"),
-- and a final card ("the river" or "fifth street"). Each player seeks the best five card poker hand
-- from any combination of the seven cards; the five community cards and their two hole cards.
-- ...
module Poker.Holdem
  (
    Hole(..)
  , Flop(..)
  , Turn(..)
  , Street(..)
  , Community(..)
  , Hand(..)
  , randomHand
  ) where

import           Data.Random.RVar (RVar)
import           Poker.Deck (Card, draw1_, shuffle, stdDeck)

-- | Player's 2 cards.
data Hole = Hole !Card !Card deriving stock (Show, Eq)

-- | First three community cards
data Flop = Flop !Card !Card !Card deriving stock (Show, Eq)

-- | Fourth community card.
newtype Turn = Turn Card deriving stock (Show, Eq)

-- | Fifth and last community card.
newtype Street = Street Card deriving stock (Show, Eq)

-- | All community cards.
data Community = Community !Flop !Turn !Street deriving stock (Show, Eq)

-- | A 7-card hand is made from a hole and community cards.
data Hand = Hand !Hole !Community deriving stock (Show, Eq)


-- | A random hand
randomHand :: RVar Hand
randomHand = do
  cards <- draw1_ 7 <$> shuffle stdDeck
  case cards of
    (Just [c1, c2, c3, c4, c5, c6, c7]) -> return $ Hand (Hole c1 c2) (Community (Flop c3 c4 c5) (Turn c6) (Street c7))
    _                                   -> undefined

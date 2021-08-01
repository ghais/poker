-- |
-- Module      : Poker.Deck
-- Description : A deck of cards and its operations.
-- Copyright   : (c) Ghais Issa, 2021
module Poker.Deck
  (
    Suit(..)
  , Rank(..)
  , Card(..)
  , newCard
  , randomCard
  , Deck(..)
  , stdDeck
  , shuffleT
  , shuffle
  , draw
  , draw_
  , draw1
  , draw1_
  , remove
  ) where

import           Data.Bits (shift, (.&.))
import           Data.List (foldl1', (\\))
import           Data.Random (RVar)
import           Data.Random.Distribution.Uniform (uniform)
import           Data.Random.List (shuffleN, shuffleNT)
import           Data.Random.RVar (RVarT)
import           Text.Read (Lexeme (Ident), Read (readPrec), lexP)

data Suit
  = Club
  | Diamond
  | Heart
  | Spade
  deriving stock (Read, Show, Eq, Enum, Ord)

data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving stock (Read, Show, Eq, Enum, Ord)

-- | A card is represented as an int where the lower 4 bits representing the
-- and the rest represents the rank ranging from 0-12.
--
-- Use 'newCard' to construct a new card.
newtype Card = Card Int deriving stock (Eq, Ord)

-- | Construct a new card.
newCard :: Rank -> Suit -> Card
newCard rank suite = Card $ fromEnum rank * 4 + fromEnum suite


randomCard :: RVar Card
randomCard = do
  suit   <- uniform 0 3
  rank   <- uniform 0 12
  return $ newCard (toEnum rank) (toEnum suit)


data Deck = Deck !Int ![Card]

-- | construct a full 52-card playing deck. The resulting deck is not shuffled.
stdDeck :: Deck
stdDeck =
  let suits = enumFrom Club
      ranks = enumFrom Two
   in Deck 52 [newCard rank suit | suit <- suits, rank <- ranks]

-- | Shuffle a deck.
shuffleT :: Deck -> RVarT m Deck
shuffleT (Deck n cards) = do
  shuffledCards <- shuffleNT n cards
  return $ Deck n shuffledCards

-- | Shuffle a deck.
shuffle :: Deck -> RVar Deck
shuffle (Deck n cards) = do
  shuffledCards <- shuffleN n cards
  return $ Deck n shuffledCards

-- | Remove a set of cards from a deck, returning the new deck.
remove :: [Card] -> Deck -> Deck
remove cards (Deck n deck) = Deck (n - length cards) (deck \\ cards)

-- | Draw a list of cards from the deck and group them based on the list of hands provided.
--
-- Returns the grouped cards and the remaining deck.
--
-- Arguments that are negative or exceed the length of the deck return Nothing.
--
-- For instance, to simulate a two player Hold'em game, one might wish
-- to draw two cards for each player, and five cards for the community:
--
-- >>> deck <- runRVar (shuffle stdDeck) DevRandom
-- >>> fst . fromJust $ draw [2,2,5] deck
-- [[Ace Club,Queen Club],[Four Diamond,Nine Club],[Jack Heart,King Diamond,Three Heart,Four Club,Two Diamond]]
draw ::
  -- | a list of hand sizes.
  [Int] ->
  -- | The deck.
  Deck ->
  -- | Nothing if the requested number of cards exceeds the deck size, or any of the hands is negative
  -- otherwise returns the hands and the remainder of the deck.
  Maybe ([[Card]], Deck)
draw handSizeLst (Deck n deck)
  | let anyNeg = any (< 0) handSizeLst
     in total > n || total < 0 || anyNeg =
    Nothing
  | otherwise =
    let draw2 [] (houtput, doutput) = (reverse houtput, doutput)
        draw2 (nToTake : hst) (handOutput, deckOutput) =
          let newHand = take nToTake deckOutput
              newDeck = drop nToTake deckOutput
           in draw2 hst (newHand : handOutput, newDeck)
        (hands, remainder) = draw2 handSizeLst ([], deck)
     in Just (hands, Deck (n - total) remainder)
  where
    total = foldl1' (+) handSizeLst


-- | Just like 'draw' but throws away the deck.
draw_ :: [Int] -> Deck -> Maybe [[Card]]
draw_ handSizes (Deck n deck) =
  let f (Just (h, _)) = Just h
      f _             = Nothing
   in f $ draw handSizes (Deck n deck)

-- | The same as 'draw', except draw only one hand of specified size.
draw1 :: Int -> Deck -> Maybe ([Card], Deck)
draw1 handSize (Deck n deck) =
  let f (Just ([h], d)) = Just (h, d)
      f _               = Nothing
   in f $ draw [handSize] (Deck n deck)

-- | Same as 'draw1' but throws away the deck.
draw1_ :: Int -> Deck -> Maybe [Card]
draw1_ handSize (Deck n deck) =
  let f (Just ([h], _)) = Just h
      f _               = Nothing
   in f $ draw [handSize] (Deck n deck)

-- A card is represented as an Int. So we implement Show and Read explicitly.
instance Show Card where
  show (Card c) = show rank ++ " " ++ show suit
    where
      suit :: Suit
      suit = toEnum (c .&. 0x3)
      rank :: Rank
      rank = toEnum $ (c - fromEnum suit) `shift` (-2)

instance Read Card where
  readPrec = do
    (Ident rank) <- lexP
    (Ident suit) <- lexP
    return $ newCard (read rank) (read suit)

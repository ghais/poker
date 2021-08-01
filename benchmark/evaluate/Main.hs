module Main (main) where

import Gauge.Main
import Poker.Holdem
import Poker.Holdem.Evaluate
import Poker.Deck



hands =
  [
    (
      Hand (Hole (newCard Ace Spade) (newCard King Spade))
         (Community
            (Flop (newCard Queen Spade) (newCard Jack Spade) (newCard Ten Spade))
            (Turn (newCard Two Diamond))
            (Street (newCard Three Diamond))
         )
    , "Royal flush"
    )
  ,
    (
      Hand (Hole (newCard Nine Diamond) (newCard Eight Diamond))
         (Community
            (Flop (newCard King Spade) (newCard King Heart) (newCard Five Diamond))
            (Turn (newCard Seven Diamond))
            (Street (newCard Six Diamond))
         )
    , "Straight flush"
    )
  , (
      Hand (Hole (newCard Two Club) (newCard Two Diamond))
         (Community
            (Flop (newCard Ace Heart) (newCard Ace Diamond) (newCard King Heart))
            (Turn (newCard Ace Spade))
            (Street (newCard Ace Club))
         )
    , "Four of a kind"
    )
  , (
      Hand (Hole (newCard Two Club) (newCard Three Diamond))
         (Community
            (Flop (newCard Ace Heart) (newCard Ace Diamond) (newCard Four Spade))
            (Turn (newCard Five Diamond))
            (Street (newCard Ace Club))
         )
    , "Straight"
    )
  , (
      Hand (Hole (newCard Two Club) (newCard Two Diamond))
         (Community
            (Flop (newCard Ace Heart) (newCard Ace Diamond) (newCard Four Spade))
            (Turn (newCard Five Diamond))
            (Street (newCard Ace Club))
         )
    , "Full House"
    )
  , (
      Hand (Hole (newCard Two Club) (newCard Two Diamond))
         (Community
            (Flop (newCard Ace Heart) (newCard Four Diamond) (newCard Five Spade))
            (Turn (newCard Seven Diamond))
            (Street (newCard King Club))
         )
    , "One Pair"
    )
  , (
      Hand (Hole (newCard Two Club) (newCard Five Diamond))
         (Community
            (Flop (newCard Seven Heart) (newCard Nine Club) (newCard Jack Spade))
            (Turn (newCard Seven Diamond))
            (Street (newCard King Club))
         )
    , "High Card"
    )
  ]
benchEvaluate (hand, description) = bench description $ whnf evaluateHand hand

main = defaultMain
  [
    bgroup "evaluate" (map benchEvaluate hands)
  ]

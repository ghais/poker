module Main (main) where

import Gauge.Main
import Poker.Deck
import Poker.Holdem
import Poker.Holdem.Simulate



game = Game
  {
    flop = Just (Flop (newCard King Diamond) (newCard Queen Diamond) (newCard Jack Spade))
  , turn = Nothing
  , street = Nothing
  , players = [
        Player (Just $ newCard Ace Diamond) (Just $ newCard Ace Spade)
      , Player Nothing Nothing
      , Player Nothing Nothing
      , Player Nothing Nothing
      ]
  }

main = defaultMain
  [
    bench "simulate 100" $ whnfAppIO (simulate 100) game
  , bench "simulate 1000" $ whnfAppIO (simulate 1000) game
  , bench "simulate 10000" $ whnfAppIO (simulate 10000) game
  ]

module Main where

import Test.Hspec
import Poker.Holdem
import Poker.Deck
import Poker.Holdem.Evaluate
import Data.List.Split (splitOn)
readLine :: [Int] -> (Hand, HandRank)
readLine [c1, c2, c3, c4, c5, c6, c7, rank] =
  (
    Hand (Hole (Card c1) (Card c2)) ( Community (Flop (Card c3) (Card c4) (Card c5)) (Turn (Card c6)) (Street (Card c7)))
  , HandRank  rank
  )

testTable = do
  let filepath =  "test/data/evaluate_holdem.csv"
  content <- tail . lines <$> readFile filepath
  return $ map (readLine . (map read) . splitOn ",") content


test (hand, rank) = do
  context (show hand) $ do
    it ("should have rank: " ++ show rank) $ do
      evaluateHand hand `shouldBe` rank

main = do
  tests <- testTable
  hspec $
    describe "Poker.Holdem.Evaluate.evaluate" $ do
      mapM_ test tests

# Poker

[![Haskell CI](https://github.com/ghais/poker/actions/workflows/haskell.yml/badge.svg)](https://github.com/ghais/poker/actions/workflows/haskell.yml)
[![Build Status](https://travis-ci.com/ghais/poker.svg?branch=main)](https://travis-ci.com/ghais/poker)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Monte Carlo simulator for Texas Hold'em that can simulate each player probability of winning.

The simulation can proceed for any number of known and unknown cards, for example we can run
a 10000 trajectory simulation of a 3 player game, where:
1. We know that the first player has Ace of Hear, and Ten of Diamond
2. The second player we only know that they hold the Ace of Diamond and one unknown card
3. For the third player we know neither of the two cards. 
4. In terms of community cards we only know the flop is King of Diamond, Queen of Diamond, and Jack of Spade

``` haskell
example :: IO ()
example = do
  let game = Game
        {
          players =
            [
              Player (Just $ newCard Ace Heart) (Just $ newCard Ten Diamond )
            , Player (Just $ newCard Ace Diamond) Nothing
            , Player Nothing Nothing
            ]
        , flop = Just (Flop (newCard King Diamond) (newCard Queen Diamond) (newCard Jack Spade))
        , turn = Nothing
        , street = Nothing
        }
  probabilities <- simulate 10000 game
  print probabilities
```

Then run ```example``` in ghci

```
λ> example
[0.7717,0.1831,4.52e-2]
λ> 
```


## Hand Evaluation
Contains an implementation of an efficient poker hand evaluation based on the work of Henry Lee which you can find at [PokerHandEvaluator](https://github.com/HenryRLee/PokerHandEvaluator)

The implementation is relatively efficient and can evaluate all possible 133,784,560 possible poker hands in less than 10 seconds.

<pre>benchmarked <font color="#4E9A06">**evaluate/Royal flush**</font>
<font color="#C4A000">**time** </font> <font color="#CC0000">   **43.72 ns** </font> (37.72 ns .. 48.69 ns)
          <font color="#CC0000">   **0.791 R²** </font> (0.543 R² .. 0.988 R²)
<font color="#C4A000">**mean** </font> <font color="#CC0000">   **52.28 ns** </font> (47.82 ns .. 62.74 ns)
<font color="#C4A000">**std dev** </font> <font color="#CC0000">**23.79 ns** </font> (13.23 ns .. 38.87 ns)
variance introduced by outliers: 97% (severely inflated)

benchmarked <font color="#4E9A06">**evaluate/Straight flush**</font>
<font color="#C4A000">**time** </font> <font color="#CC0000">   **47.09 ns** </font> (45.88 ns .. 48.31 ns)
 <font color="#CC0000">            **0.994 R²** </font> (0.984 R² .. 0.997 R²)
<font color="#C4A000">**mean** </font> <font color="#CC0000">   **47.44 ns** </font> (46.84 ns .. 48.83 ns)
<font color="#C4A000">**std dev** </font> <font color="#CC0000">**2.718 ns** </font> (1.640 ns .. 5.042 ns)
variance introduced by outliers: 34% (moderately inflated)

benchmarked <font color="#4E9A06">**evaluate/Four of a kind**</font>
<font color="#C4A000">**time** </font> <font color="#CC0000">   **73.26 ns** </font> (71.52 ns .. 74.99 ns)
 <font color="#CC0000">            **0.994 R²** </font> (0.989 R² .. 0.997 R²)
<font color="#C4A000">**mean** </font> <font color="#CC0000">   **75.20 ns** </font> (74.22 ns .. 76.42 ns)
<font color="#C4A000">**std dev** </font> <font color="#CC0000">**3.749 ns** </font> (2.918 ns .. 4.649 ns)
variance introduced by outliers: 27% (moderately inflated)

benchmarked <font color="#4E9A06">**evaluate/Straight**</font>
<font color="#C4A000">**time** </font> <font color="#CC0000">   **80.03 ns** </font> (77.01 ns .. 83.81 ns)
 <font color="#CC0000">            **0.990 R²** </font> (0.977 R² .. 0.998 R²)
<font color="#C4A000">**mean** </font> <font color="#CC0000">   **78.44 ns** </font> (77.69 ns .. 79.68 ns)
<font color="#C4A000">**std dev** </font> <font color="#CC0000">**3.088 ns** </font> (1.894 ns .. 5.316 ns)
variance introduced by outliers: 20% (moderately inflated)

benchmarked <font color="#4E9A06">**evaluate/Full House**</font>
<font color="#C4A000">**time** </font> <font color="#CC0000">   **82.80 ns** </font> (75.65 ns .. 87.82 ns)
 <font color="#CC0000">            **0.972 R²** </font> (0.953 R² .. 0.988 R²)
<font color="#C4A000">**mean** </font> <font color="#CC0000">   **80.08 ns** </font> (77.53 ns .. 84.31 ns)
<font color="#C4A000">**std dev** </font> <font color="#CC0000">**10.48 ns** </font> (6.545 ns .. 15.71 ns)
variance introduced by outliers: 75% (severely inflated)

benchmarked <font color="#4E9A06">**evaluate/One Pair**</font>
<font color="#C4A000">**time** </font> <font color="#CC0000">   **77.32 ns** </font> (74.52 ns .. 80.17 ns)
 <font color="#CC0000">            **0.994 R²** </font> (0.990 R² .. 0.998 R²)
<font color="#C4A000">**mean** </font> <font color="#CC0000">   **74.48 ns** </font> (73.38 ns .. 75.44 ns)
<font color="#C4A000">**std dev** </font> <font color="#CC0000">**3.266 ns** </font> (2.522 ns .. 4.255 ns)
variance introduced by outliers: 24% (moderately inflated)

benchmarked <font color="#4E9A06">**evaluate/High Card**</font>
<font color="#C4A000">**time** </font> <font color="#CC0000">   **75.50 ns** </font> (71.19 ns .. 80.80 ns)
 <font color="#CC0000">            **0.912 R²** </font> (0.733 R² .. 0.999 R²)
<font color="#C4A000">**mean** </font> <font color="#CC0000">   **76.31 ns** </font> (73.65 ns .. 88.16 ns)
<font color="#C4A000">**std dev** </font> <font color="#CC0000">**14.98 ns** </font> (2.245 ns .. 33.66 ns)
variance introduced by outliers: 87% (severely inflated)
</pre>

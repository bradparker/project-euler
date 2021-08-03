{-# OPTIONS_GHC -Wall #-}

module Main where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import Solution (leastCommonMultiple, leastCommonMultiple')

main :: IO ()
main =
  defaultMain
    [ bgroup
        "leastCommonMultiple"
        [ bench "10" $ whnf leastCommonMultiple [1 .. 10],
          bench "100" $ whnf leastCommonMultiple [1 .. 100],
          bench "1000" $ whnf leastCommonMultiple [1 .. 1000],
          bench "10000" $ whnf leastCommonMultiple [1 .. 10000]
        ],
      bgroup
        "leastCommonMultiple'"
        [ bench "10" $ whnf leastCommonMultiple' [1 .. 10],
          bench "100" $ whnf leastCommonMultiple' [1 .. 100],
          bench "1000" $ whnf leastCommonMultiple' [1 .. 1000],
          bench "10000" $ whnf leastCommonMultiple' [1 .. 10000]
        ],
      bgroup
        "fold lcm"
        [ bench "10" $ whnf (foldl1 lcm) [1 .. 10],
          bench "100" $ whnf (foldl1 lcm) [1 .. 100],
          bench "1000" $ whnf (foldl1 lcm) [1 .. 1000],
          bench "10000" $ whnf (foldl1 lcm) [1 .. 10000]
        ]
    ]

{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- Starting in the top left corner of a 2×2 grid, and only being able
-- to move to the right and down, there are exactly 6 routes to the
-- bottom right corner.
--
-- How many such routes are there through a 20×20 grid?

import Numeric.Natural (Natural)

choose :: Natural -> Natural -> Natural
choose n k =
  product (take (fromIntegral k) [n, (n - 1) .. 1])
    `div` product [1 .. k]

main :: IO ()
main = print (40 `choose` 20)

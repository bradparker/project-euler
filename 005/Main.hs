{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- 2520 is the smallest number that can be divided by each of the
-- numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all
-- of the numbers from 1 to 20?

import qualified Data.IntMap as IntMap
import Numeric.Natural (Natural)
import Primes (primePowers)

solve :: [Natural] -> Int
solve =
  product
    . map (uncurry (^))
    . IntMap.assocs
    . foldr (IntMap.unionWith max . primePowers) IntMap.empty

main :: IO ()
main = print (solve [1 .. 20])

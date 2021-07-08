{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- 2520 is the smallest number that can be divided by each of the
-- numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all
-- of the numbers from 1 to 20?

import qualified Data.Map as Map
import Numeric.Natural (Natural)
import Primes (primePowers)

leastCommonMultiple :: [Natural] -> Natural
leastCommonMultiple =
  product
    . map (uncurry (^))
    . Map.assocs
    . foldr
      ( Map.unionWith max
          . Map.fromAscList
          . primePowers
      )
      Map.empty

main :: IO ()
main = print (leastCommonMultiple [1 .. 20])

{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Solution
  ( leastCommonMultiple,
    leastCommonMultiple',
  )
where

-- 2520 is the smallest number that can be divided by each of the
-- numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all
-- of the numbers from 1 to 20?

import Data.Foldable (foldl')
import qualified Data.Map as Map
import qualified Data.Map.Strict as SMap
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

leastCommonMultiple' :: [Natural] -> Natural
leastCommonMultiple' =
  product'
    . map (uncurry (^))
    . SMap.assocs
    . foldl'
      ( flip
          ( SMap.unionWith max
              . SMap.fromAscList
              . primePowers
          )
      )
      SMap.empty
  where
    product' = foldl' (*) 1

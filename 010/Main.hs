{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.

import Data.Foldable (foldl')
import Primes (primes)

sum' :: forall a t. (Num a, Foldable t) => t a -> a
sum' = foldl' (+) 0

solve :: Int -> Int
solve n = sum' (takeWhile (< n) primes)

main :: IO ()
main = print (solve 2_000_000)

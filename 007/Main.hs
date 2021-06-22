{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Primes (primes)

-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we
-- can see that the 6th prime is 13.
-- What is the 10,001st prime number?

solve :: Int -> Int
solve = (primes !!) . subtract 1

main :: IO ()
main = print (solve 10_001)

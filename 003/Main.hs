{-# OPTIONS_GHC -Wall #-}

module Main where

import Numeric.Natural (Natural)
import Primes (primeFactors)

-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

solve :: Natural -> Natural
solve = maximum . primeFactors

solution :: Natural
solution = solve 600851475143

main :: IO ()
main = print solution

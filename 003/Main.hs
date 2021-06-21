{-# OPTIONS_GHC -Wall #-}

module Main where

import Primes (primeFactors)

-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

solve :: Int -> Int
solve = maximum . primeFactors

solution :: Int
solution = solve 600851475143

main :: IO ()
main = print solution

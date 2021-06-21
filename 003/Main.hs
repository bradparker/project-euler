{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.List (unfoldr)

-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

primes :: [Int]
primes = 2 : unfoldr sieve [3, 5 ..]
  where
    sieve :: [Int] -> Maybe (Int, [Int])
    sieve [] = Nothing
    sieve (n : ns) = Just (n, filter ((0 /=) . (`mod` n)) ns)

primeFactors :: Int -> [Int]
primeFactors = go primes
  where
    go [] _ = [] -- urgh
    go _ 1 = []
    go (p : ps) n =
      let (n', r) = divMod n p
       in if r == 0
            then p : go (p : ps) n'
            else go ps n

solve :: Int -> Int
solve = maximum . primeFactors

solution :: Int
solution = solve 600851475143

main :: IO ()
main = print solution

{-# OPTIONS_GHC -Wall #-}

module Primes where

import Data.List (unfoldr)

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

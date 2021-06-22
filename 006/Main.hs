{-# OPTIONS_GHC -Wall #-}

module Main where

-- The sum of the squares of the first ten natural numbers is,
-- 1^2 + 2^2 + ... + 10^2 = 385
-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)^2 = 3025
-- Hence the difference between the sum of the squares of the first ten
-- natural numbers and the square of the sum is 3025 - 385 = 2640.
-- Find the difference between the sum of the squares of the first one
-- hundred natural numbers and the square of the sum.

solve :: Integral n => n -> n
solve n = sum [1 .. n] ^ (2 :: Int) - sum (map (^ (2 :: Int)) [1 .. n])

main :: IO ()
main = print (solve 100 :: Int)

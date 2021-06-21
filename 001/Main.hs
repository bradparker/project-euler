{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Function ((&))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

-- If we list all the natural numbers below 10 that are multiples of 3
-- or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

multiplesOf :: Num n => n -> [n]
multiplesOf n = iterate (+ n) n

below :: Int -> [Int] -> IntSet
below bound = IntSet.fromAscList . takeWhile (< bound)

sum' :: IntSet -> Int
sum' = IntSet.foldl' (+) 0

solve :: Int -> Int
solve bound =
  sum'
    ( (multiplesOf 3 & below bound)
        <> (multiplesOf 5 & below bound)
    )

solution :: Int
solution = solve 1000

main :: IO ()
main = print solution

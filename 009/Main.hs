{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
-- a^2 + b^2 = c^2
--
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

import Control.Monad (guard)
import Data.Maybe (listToMaybe)

solve :: Int -> Maybe Int
solve n = listToMaybe do
  a <- [1 .. n]
  b <- [(a + 1) .. (n - a)]
  let c = n - (a + b)
  guard (a * a + b * b == c * c)
  pure (a * b * c)

main :: IO ()
main = print (solve 1000)

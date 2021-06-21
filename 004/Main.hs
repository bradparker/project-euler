{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- A palindromic number reads the same both ways. The largest
-- palindrome made from the product of two 2-digit numbers is
-- 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit
-- numbers.

import Control.Monad.State (StateT (StateT), evalStateT)
import Data.List (find, sortOn, unfoldr, uncons)
import Data.Ord (Down (Down))

unconses :: [a] -> [(a, [a])]
unconses = unfoldr \as ->
  case uncons as of
    Nothing -> Nothing
    Just (a, as') -> Just ((a, as'), as')

threeDigitProducts :: [Int]
threeDigitProducts = evalStateT ((*) <$> StateT unconses <*> StateT unconses) [100 .. 999]

digits :: Int -> [Int]
digits = unfoldr \n ->
  case divMod n 10 of
    (0, 0) -> Nothing
    (n', r) -> Just (r, n')

isPalindrome :: Int -> Bool
isPalindrome n = let ds = digits n in ds == reverse ds

solution :: Maybe Int
solution = find isPalindrome (sortOn Down threeDigitProducts)

main :: IO ()
main = print solution

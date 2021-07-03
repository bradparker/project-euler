{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- The following iterative sequence is defined for the set of positive
-- integers:
--
-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following
-- sequence:
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
--
-- It can be seen that this sequence (starting at 13 and finishing at
-- 1) contains 10 terms. Although it has not been proved yet (Collatz
-- Problem), it is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one
-- million.

import Control.Monad.State (State, execState, gets, modify)
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (maximumBy)

collatz :: Int -> Int
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

collatzSizes :: [Int] -> IntMap Int
collatzSizes ns = execState (traverse collatzSize ns) IntMap.empty
  where
    collatzSize :: Int -> State (IntMap Int) Int
    collatzSize 1 = pure 1
    collatzSize n = do
      existing <- gets (IntMap.lookup n)
      case existing of
        Nothing -> do
          size <- (1 +) <$> collatzSize (collatz n)
          modify (IntMap.insert n size)
          pure size
        Just size -> pure size

solve :: Int -> Int
solve n =
  fst
    . maximumBy (on compare snd)
    . IntMap.assocs
    . fst
    . IntMap.split n
    . collatzSizes
    . enumFromTo 2
    $ (n - 1)

main :: IO ()
main = print (solve 1000000)

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
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
import Data.Functor.Base (ListF (Cons, Nil), TreeF (NodeF))
import Data.Functor.Foldable (unfold)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (maximumBy)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree)
import Numeric.Natural (Natural)

collatzSizes :: [Int] -> IntMap Int
collatzSizes ns = execState (traverse collatzSize ns) IntMap.empty
  where
    collatzSize :: Int -> State (IntMap Int) Int
    collatzSize 1 = pure 1
    collatzSize n = do
      existing <- gets (IntMap.lookup n)
      case existing of
        Nothing -> do
          size <- (1 +) <$> collatzSize (fromIntegral (collatz (fromIntegral n)))
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

collatz :: Natural -> Natural
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

collatzSequence :: Natural -> [Natural]
collatzSequence n0 = n0 : unfold alg n0
  where
    alg :: Natural -> ListF Natural Natural
    alg 1 = Nil
    alg n = let c = collatz n in Cons c c

invertedCollatz :: Natural -> Tree Natural
invertedCollatz bound = unfold alg (1, Set.singleton 1)
  where
    alg :: (Natural, Set Natural) -> TreeF Natural (Natural, Set Natural)
    alg (n, seen) =
      NodeF
        n
        ( if n > bound
            then []
            else
              map
                (,Set.fromList next <> seen)
                (filter (not . (`Set.member` seen)) next)
        )
      where
        next :: [Natural]
        next = n * 2 : [(n - 1) `div` 3 | n > 1 && (n - 1) `mod` 3 == 0]

-- >>> collatzSequence 13
-- [13,40,20,10,5,16,8,4,2,1]
-- >>> putStrLn (Tree.drawTree (fmap show (invertedCollatz 40)))
--  1
--  |
--  `- 2
--     |
--     `- 4
--        |
--        `- 8
--           |
--           `- 16
--              |
--              +- 32
--              |  |
--              |  `- 64
--              |
--              `- 5
--                 |
--                 `- 10
--                    |
--                    +- 20
--                    |  |
--                    |  `- 40
--                    |     |
--                    |     +- 80
--                    |     |
--                    |     `- 13
--                    |        |
--                    |        `- 26
--                    |           |
--                    |           `- 52
--                    |
--                    `- 3
--                       |
--                       `- 6
--                          |
--                          `- 12
--                             |
--                             `- 24
--                                |
--                                `- 48
main :: IO ()
main = print (solve 1000000)

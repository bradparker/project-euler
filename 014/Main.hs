{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (State, execState, gets, modify)
import Data.Function (on)
import Data.Functor.Base (ListF (Cons, Nil), TreeF (NodeF))
import Data.Functor.Foldable (fold, unfold)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (genericLength, maximumBy)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree (Node))
import qualified Data.Tree as Tree
import Numeric.Natural (Natural)

collatzSizes :: [Int] -> IntMap Int
collatzSizes ns = execState (traverse collatzSize' ns) IntMap.empty
  where
    collatzSize' :: Int -> State (IntMap Int) Int
    collatzSize' 1 = pure 1
    collatzSize' n = do
      existing <- gets (IntMap.lookup n)
      case existing of
        Nothing -> do
          size <- (1 +) <$> collatzSize' (collatz n)
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

collatz :: Integral n => n -> n
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

collatzSequence :: forall n. Integral n => n -> [n]
collatzSequence n0 = n0 : unfold alg n0
  where
    alg :: n -> ListF n n
    alg 1 = Nil
    alg n = let c = collatz n in Cons c c

acyclicInvertedCollatz :: Tree Natural
acyclicInvertedCollatz = unfold alg (1, Set.singleton 1)
  where
    alg :: (Natural, Set Natural) -> TreeF Natural (Natural, Set Natural)
    alg (n, seen) =
      NodeF
        n
        ( map
            (,Set.fromList next <> seen)
            (filter (not . (`Set.member` seen)) next)
        )
      where
        next :: [Natural]
        next = n * 2 : [(n - 1) `div` 3 | n > 1 && (n - 1) `mod` 3 == 0]

invertedCollatz :: Tree Natural
invertedCollatz = unfold alg 1
  where
    alg :: Natural -> TreeF Natural Natural
    alg n = NodeF n next
      where
        next :: [Natural]
        next = n * 2 : [(n - 1) `div` 3 | n > 1 && (n - 1) `mod` 3 == 0]

toLevel :: forall a. Natural -> Tree a -> Tree a
toLevel target = flip runReader 0 . fold alg
  where
    alg :: TreeF a (Reader Natural (Tree a)) -> Reader Natural (Tree a)
    alg (NodeF a as) = do
      level <- ask
      if level >= target
        then pure (Node a [])
        else Node a <$> traverse (local (+ 1)) as

invertedCollatzLevels :: [Set Natural]
invertedCollatzLevels = Set.fromList <$> Tree.levels invertedCollatz

collatzSize :: Natural -> Natural
collatzSize n = genericLength (takeWhile (Set.notMember n) invertedCollatzLevels)

-- >>> collatzSequence 13
-- [13,40,20,10,5,16,8,4,2,1]
-- >>> putStrLn (Tree.drawTree (fmap show (toLevel 9 acyclicInvertedCollatz)))
--   1
--   |
--   `- 2
--      |
--      `- 4
--         |
--         `- 8
--            |
--            `- 16
--               |
--               +- 32
--               |  |
--               |  `- 64
--               |     |
--               |     +- 128
--               |     |  |
--               |     |  `- 256
--               |     |     |
--               |     |     +- 512
--               |     |     |
--               |     |     `- 85
--               |     |
--               |     `- 21
--               |        |
--               |        `- 42
--               |           |
--               |           `- 84
--               |
--               `- 5
--                  |
--                  `- 10
--                     |
--                     +- 20
--                     |  |
--                     |  `- 40
--                     |     |
--                     |     +- 80
--                     |     |
--                     |     `- 13
--                     |
--                     `- 3
--                        |
--                        `- 6
--                           |
--                           `- 12
main :: IO ()
main = print (solve 1000000)

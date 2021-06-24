{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- 2520 is the smallest number that can be divided by each of the
-- numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all
-- of the numbers from 1 to 20?

import Control.Arrow ((&&&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (sortOn, uncons)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (listToMaybe)
import Data.Ord (Down (Down))
import Numeric.Natural (Natural)
import Primes (primeFactors)

solve_1 :: Integral n => [n] -> Maybe n
solve_1 = listToMaybe . foldr (\n -> filter ((0 ==) . (`mod` n))) [2, 4 ..]

-- I think this is actually slower?

multiples :: Num a => a -> [a]
multiples n = iterate (+ n) n

solve_2 :: forall a. (Ord a, Num a) => [a] -> Maybe a
solve_2 ns = uncurry go =<< uncons (map multiples (sortOn Down ns))
  where
    go :: [a] -> [[a]] -> Maybe a
    go [] _ = Nothing
    go _ [] = Nothing
    go (x : xs) xss =
      let xss' = map (dropWhile (< x)) xss
       in if all ((Just x ==) . listToMaybe) xss'
            then Just x
            else go xs xss'

-- Cheers Wikipedia!
-- https://en.wikipedia.org/wiki/Least_common_multiple#Using_prime_factorization

primePowers :: Natural -> IntMap Int
primePowers =
  IntMap.fromList
    . map (fromIntegral . NonEmpty.head &&& length)
    . NonEmpty.group
    . primeFactors

solve_3 :: [Natural] -> Int
solve_3 =
  product
    . map (uncurry (^))
    . IntMap.assocs
    . foldr (IntMap.unionWith max . primePowers) IntMap.empty

main :: IO ()
main = print (solve_3 [1 .. 20])

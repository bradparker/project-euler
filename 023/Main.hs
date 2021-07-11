{-# OPTIONS_GHC -Wall #-}

module Main where

-- A perfect number is a number for which the sum of its proper
-- divisors is exactly equal to the number. For example, the sum of the
-- proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means
-- that 28 is a perfect number.
--
-- A number n is called deficient if the sum of its proper divisors is
-- less than n and it is called abundant if this sum exceeds n.
--
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
-- smallest number that can be written as the sum of two abundant
-- numbers is 24. By mathematical analysis, it can be shown that all
-- integers greater than 28123 can be written as the sum of two
-- abundant numbers. However, this upper limit cannot be reduced any
-- further by analysis even though it is known that the greatest number
-- that cannot be expressed as the sum of two abundant numbers is less
-- than this limit.
--
-- Find the sum of all the positive integers which cannot be written as
-- the sum of two abundant numbers.

import Numeric.Natural (Natural)
import Primes (sigma)
import Stream (Stream ((:>)))
import qualified Stream

(\\) :: Ord a => [a] -> [a] -> [a]
(\\) [] ys = ys
(\\) xs [] = xs
(\\) (x : xs) (y : ys) =
  case compare x y of
    LT -> x : (xs \\ (y : ys))
    EQ -> xs \\ ys
    GT -> (x : xs) \\ ys

properDivisorSum :: Natural -> Natural
properDivisorSum = subtract <*> sigma

isAbundent :: Natural -> Bool
isAbundent = (<) <*> properDivisorSum

abundentNumbers :: Stream Natural
abundentNumbers = 12 :> Stream.filter isAbundent (Stream.enumFrom 18)

abundentSums :: Stream Natural
abundentSums = Stream.unionAll ((\n -> (+ n) <$> abundentNumbers) <$> abundentNumbers)

nonAbundentSums :: [Natural]
nonAbundentSums = [1 .. 28123] \\ Stream.takeWhile (< 28123) abundentSums

main :: IO ()
main = print (sum nonAbundentSums)

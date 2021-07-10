{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- n! means n × (n − 1) × ... × 3 × 2 × 1
--
-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is
-- 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
--
-- Find the sum of the digits in the number 100!

import Numeric.Natural (Natural)
import Primes (factors)

d :: Integral n => n -> Natural
d = subtract . fromIntegral <*> sum . factors . fromIntegral

isAmicable :: Natural -> Bool
isAmicable a = let b = d a in a /= b && d b == a

solve :: Natural -> Natural
solve = sum . filter isAmicable . enumFromTo 2 . subtract 1

main :: IO ()
main = print (solve 10_000)

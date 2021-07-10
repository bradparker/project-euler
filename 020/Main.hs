{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- n! means n × (n − 1) × ... × 3 × 2 × 1
--
-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is
-- 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
--
-- Find the sum of the digits in the number 100!

import Digits (digits)
import Numeric.Natural (Natural)

(!) :: (Enum a, Num a) => a -> a
(!) = product . enumFromTo 1

solve :: Natural -> Natural
solve n = sum (digits (n !))

main :: IO ()
main = print (solve 100)

{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.List (findIndex)
import Digits (digits)
import Numeric.Natural (Natural)

fibs :: [Natural]
fibs = 0 : scanl (+) 1 fibs

solve :: Int -> Maybe Int
solve n = findIndex ((n ==) . length . digits) fibs

main :: IO ()
main = print (solve 1000)

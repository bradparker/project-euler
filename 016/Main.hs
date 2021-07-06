{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Digits (digits)
import Numeric.Natural (Natural)

main :: IO ()
main = print (sum (digits (2 ^ (1000 :: Int))) :: Natural)

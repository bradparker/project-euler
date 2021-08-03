{-# OPTIONS_GHC -Wall #-}

module Main where

import Solution (leastCommonMultiple)

main :: IO ()
main = print (leastCommonMultiple [1 .. 20])

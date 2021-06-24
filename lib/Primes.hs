{-# OPTIONS_GHC -Wall #-}

module Primes where

import Numeric.Natural (Natural)
import Stream (Stream (Stream))
import qualified Stream

primeFactors :: Natural -> [Natural]
primeFactors = go primes
  where
    go _ 1 = []
    go (Stream p ps) n =
      let (n', r) = divMod n p
       in if r == 0
            then p : go (Stream p ps) n'
            else go ps n

composites :: Stream Natural
composites = Stream.mergeAll (mulitples <$> primes)
  where
    mulitples :: Natural -> Stream Natural
    mulitples n = Stream.iterate (+ n) (n * n)

primes :: Stream Natural
primes = Stream 2 (Stream.iterate (+ 1) 3 `Stream.difference` composites)

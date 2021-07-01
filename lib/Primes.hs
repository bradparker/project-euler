{-# OPTIONS_GHC -Wall #-}

module Primes where

import Numeric.Natural (Natural)
import Stream (Stream ((:>)), (\\))
import qualified Stream

primeFactors :: Natural -> [Natural]
primeFactors = go primes
  where
    go _ 1 = []
    go (p :> ps) n =
      let (n', r) = divMod n p
       in if r == 0
            then p : go (p :> ps) n'
            else go ps n

primes :: Stream Natural
primes = 2 :> (naturalsFrom 3 \\ composites)
  where
    naturalsFrom :: Natural -> Stream Natural
    naturalsFrom = Stream.enumFrom

    composites :: Stream Natural
    composites = Stream.unionAll (multiples <$> primes)

    multiples :: Natural -> Stream Natural
    multiples n = Stream.iterate (+ n) (n * n)

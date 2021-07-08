{-# OPTIONS_GHC -Wall #-}

module Primes where

import Control.Arrow ((&&&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty
import Numeric.Natural (Natural)
import Stream (Stream ((:>)), (\\))
import qualified Stream

primeFactors :: Integral n => n -> [Natural]
primeFactors = go primes
  where
    go _ (-1) = []
    go _ 1 = []
    go (p :> ps) n =
      let (n', r) = quotRem n (fromIntegral p)
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

primePowers :: Natural -> IntMap Int
primePowers =
  IntMap.fromList
    . map (fromIntegral . NonEmpty.head &&& length)
    . NonEmpty.group
    . primeFactors

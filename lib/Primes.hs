{-# LANGUAGE ExplicitForAll #-}
{-# OPTIONS_GHC -Wall #-}

module Primes where

import Control.Arrow ((&&&))
import Data.Foldable (foldl')
import Data.List (subsequences)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Stream (Stream ((:>)), (\\))
import qualified Stream

primeFactors :: Integral n => n -> [Natural]
primeFactors = go primes
  where
    go _ n | abs n == 1 = []
    go (p :> ps) n =
      let (n', r) = quotRem n (fromIntegral p)
       in if r == 0
            then p : go (p :> ps) n'
            else go ps n

factors :: Integral n => n -> Set Natural
factors = Set.fromList . map product . subsequences . primeFactors

primes :: Stream Natural
primes = 2 :> (naturalsFrom 3 \\ composites)
  where
    naturalsFrom :: Natural -> Stream Natural
    naturalsFrom = Stream.enumFrom

    composites :: Stream Natural
    composites = Stream.unionAll (multiples <$> primes)

    multiples :: Natural -> Stream Natural
    multiples n = Stream.iterate (+ n) (n * n)

primePowers :: Integral n => n -> [(Natural, Natural)]
primePowers =
  map (NonEmpty.head &&& genericLength)
    . NonEmpty.group
    . primeFactors
  where
    genericLength :: forall t a n. (Foldable t, Num n) => t a -> n
    genericLength = foldl' (const . (+ 1)) 0

sigma :: forall n. Integral n => n -> Natural
sigma =
  product'
    . map (\(p, a) -> (p ^ (a + 1) - 1) `div` (p - 1))
    . primePowers
  where
    product' :: forall t n. (Foldable t, Num n) => t n -> n
    product' = foldl' (*) 1

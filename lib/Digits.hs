{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# OPTIONS_GHC -Wall #-}

module Digits where

import Data.List (unfoldr)

digits :: forall n. Integral n => n -> [n]
digits =
  reverse . unfoldr \n ->
    case divMod n 10 of
      (0, 0) -> Nothing
      (n', r) -> Just (r, n')

undigits :: forall n. Num n => [n] -> n
undigits = sum . zipWith (*) (iterate (* 10) 1) . reverse

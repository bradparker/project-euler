{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad.State (StateT, evalStateT, gets, lift, put)
import Data.List (sort, uncons, unfoldr)

-- | Computes the digits of a base 10 number in the factorial number system
-- >>> asFactorial 463
-- [3,4,1,0,1,0]
asFactorial :: forall a. Integral a => a -> [a]
asFactorial n0 = reverse (unfoldr alg (n0, 1))
  where
    alg :: (a, a) -> Maybe (a, (a, a))
    alg (n, m) =
      let (n', r) = quotRem n m
       in if n == 0
            then Nothing
            else Just (r, (n', m + 1))

-- | Interprets a Lehmer code as a permutation
-- >>> interpretLehmer [0 .. 2] . asFactorial <$> [0..5]
-- [Just [0,1,2],Just [0,2,1],Just [1,0,2],Just [1,2,0],Just [2,0,1],Just [2,1,0]]
interpretLehmer :: forall a b. (Ord a, Integral b) => [a] -> [b] -> Maybe [a]
interpretLehmer s code = evalStateT (traverse alg padded) (sort s)
  where
    padded :: [b]
    padded = replicate (length s - length code) 0 <> code

    alg :: b -> StateT [a] Maybe a
    alg d = do
      (lt, egt) <- gets (splitAt (fromIntegral d))
      (e, gt) <- lift (uncons egt)
      put (lt <> gt)
      pure e

lexicographicPermutation :: forall a b. (Ord a, Integral b) => [a] -> b -> Maybe [a]
lexicographicPermutation s = interpretLehmer s . asFactorial . subtract 1

main :: IO ()
main = print (lexicographicPermutation @Char @Integer ['0' .. '9'] 1_000_000)

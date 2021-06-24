{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

module Stream where

import Numeric.Natural (Natural)
import Prelude hiding (iterate, take, takeWhile, (!!))

data Stream a = Stream a (Stream a)

iterate :: (a -> a) -> a -> Stream a
iterate f a = Stream a (iterate f (f a))

take :: Natural -> Stream a -> [a]
take 0 _ = []
take n (Stream x xs) = x : take (n - 1) xs

(!!) :: Stream a -> Natural -> a
Stream x xs !! 0 = x
Stream x xs !! n = xs !! (n - 1)

takeWhile :: (a -> Bool) -> Stream a -> [a]
takeWhile p (Stream x xs)
  | p x = x : takeWhile p xs
  | otherwise = []

instance Functor Stream where
  fmap :: (a -> b) -> Stream a -> Stream b
  fmap f (Stream x xs) = Stream (f x) (fmap f xs)

difference :: Ord a => Stream a -> Stream a -> Stream a
difference (Stream x xs) (Stream y ys)
  | x < y = Stream x (difference xs (Stream y ys))
  | x == y = difference xs ys
  | x > y = difference (Stream x xs) ys

merge :: Ord a => Stream a -> Stream a -> Stream a
merge (Stream x xs) (Stream y ys)
  | x < y = Stream x (merge xs (Stream y ys))
  | x == y = Stream x (merge xs ys)
  | x > y = Stream y (merge (Stream x xs) ys)

mergeAll :: Ord a => Stream (Stream a) -> Stream a
mergeAll (Stream (Stream x xs) xss) =
  Stream x (merge xs (mergeAll xss))

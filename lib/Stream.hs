{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

module Stream where

import Numeric.Natural (Natural)
import Prelude hiding (iterate, take, takeWhile, (!!))

data Stream a = a :> Stream a

iterate :: (a -> a) -> a -> Stream a
iterate f a = a :> iterate f (f a)

take :: Natural -> Stream a -> [a]
take 0 _ = []
take n (x :> xs) = x : take (n - 1) xs

(!!) :: Stream a -> Natural -> a
(x :> _) !! 0 = x
(_ :> xs) !! n = xs !! (n - 1)

takeWhile :: (a -> Bool) -> Stream a -> [a]
takeWhile p (x :> xs)
  | p x = x : takeWhile p xs
  | otherwise = []

instance Functor Stream where
  fmap :: (a -> b) -> Stream a -> Stream b
  fmap f (x :> xs) = f x :> fmap f xs

(\\) :: Ord a => Stream a -> Stream a -> Stream a
(\\) (x :> xs) (y :> ys) =
  case compare x y of
    LT -> x :> (xs \\ (y :> ys))
    EQ -> xs \\ ys
    GT -> (x :> xs) \\ ys

union :: Ord a => Stream a -> Stream a -> Stream a
union (x :> xs) (y :> ys) =
  case compare x y of
    LT -> x :> (xs `union` (y :> ys))
    EQ -> x :> (xs `union` ys)
    GT -> y :> ((x :> xs) `union` ys)

unionAll :: Ord a => Stream (Stream a) -> Stream a
unionAll ((x :> xs) :> xss) = x :> (xs `union` unionAll xss)

-- So this is unproductive and I'd like to properly understand why
--
-- fold :: (a -> b -> b) -> Stream a -> b
-- fold f (x :> xs) = x `f` fold f xs

-- unionAll' :: Ord a => Stream (Stream a) -> Stream a
-- unionAll' = fold (\(x :> xs) xss -> x :> xs `union` xss)

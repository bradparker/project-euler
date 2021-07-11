{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

module Stream where

import Numeric.Natural (Natural)
import Prelude hiding (filter, iterate, take, takeWhile, (!!))

data Stream a = a :> Stream a

uncons :: forall a. Stream a -> (a, Stream a)
uncons (x :> xs) = (x, xs)

head :: forall a. Stream a -> a
head = fst . uncons

tail :: forall a. Stream a -> Stream a
tail = snd . uncons

unfold :: forall a b. (a -> (b, a)) -> a -> Stream b
unfold f a = let (b, a') = f a in b :> unfold f a'

fold :: forall a b. (a -> b -> b) -> Stream a -> b
fold f (x :> xs) = x `f` fold f xs

iterate :: forall a. (a -> a) -> a -> Stream a
iterate f = unfold (\a -> (a, f a))

take :: forall a. Natural -> Stream a -> [a]
take 0 _ = []
take n (x :> xs) = x : take (n - 1) xs

(!!) :: Stream a -> Natural -> a
(x :> _) !! 0 = x
(_ :> xs) !! n = xs !! (n - 1)

takeWhile :: (a -> Bool) -> Stream a -> [a]
takeWhile p = fold (\x acc -> if p x then x : acc else [])

enumFrom :: Enum a => a -> Stream a
enumFrom = iterate succ

filter :: (a -> Bool) -> Stream a -> Stream a
filter p (x :> xs) = if p x then x :> filter p xs else filter p xs

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
unionAll' :: Ord a => Stream (Stream a) -> Stream a
unionAll' = fold (\(x :> xs) xss -> x :> xs `union` xss)

-- multiples = (\n -> iterate (+ n) n)
-- iterate = (\f a -> unfold (\a -> (a, f a)) a)
-- unfold = (\f a -> let (b, a') = f a in b :> unfold f a')
--
-- Start:
-- unionAll (multiples <$> enumFrom 1)
--
-- Inline `unionAll`:
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) (multiples <$> enumFrom 1)
--
-- Inline `<$>`:
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\f (x :> xs) -> f x :> (f <$> xs)) multiples (enumFrom 1))
--
-- Evaluate `(multiples <$>)`:
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) (enumFrom 1))
--
-- Inline `multiples`:
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> (\n -> iterate (+ n) n) x :> (multiples <$> xs)) (enumFrom 1))
--
-- Evaluate `(\n -> iterate (+ n) n) x`:
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> iterate (+ x) x :> (multiples <$> xs)) (enumFrom 1))
--
-- Inline `enumFrom`:
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> iterate (+ x) x :> (multiples <$> xs)) (iterate succ 1))
--
-- Inline `iterate`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> iterate (+ x) x :> (multiples <$> xs)) ((\f a -> unfold (\a -> (a, f a)) a) succ 1))
--
-- Evaluate `(\f a -> unfold (\a -> (a, f a))) succ`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> iterate (+ x) x :> (multiples <$> xs)) ((\a -> unfold (\a -> (a, succ a)) a) 1))
--
-- Evaluate `(\a -> unfold (\a -> (a, succ a))) 1`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> iterate (+ x) x :> (multiples <$> xs)) (unfold (\a -> (a, succ a)) 1))
--
-- Inline `unfold`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> iterate (+ x) x :> (multiples <$> xs)) ((\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, succ a)) 1))
--
-- Evaluate `(\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, succ a))`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> iterate (+ x) x :> (multiples <$> xs)) ((\a -> let (b, a') = (\a -> (a, succ a)) a in b :> unfold (\a -> (a, succ a)) a') 1))
--
-- Evaluate `(\a -> (a, succ a)) a`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> iterate (+ x) x :> (multiples <$> xs)) ((\a -> let (b, a') = (a, succ a) in b :> unfold (\a -> (a, succ a)) a') 1))
--
-- Inline `b` and `a'`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> iterate (+ x) x :> (multiples <$> xs)) ((\a -> a :> unfold (\a -> (a, succ a)) (succ a)) 1))
--
-- Evaluate `(\a -> a :> unfold (\a -> (a, succ a)) (succ a)) 1`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> iterate (+ x) x :> (multiples <$> xs)) (1 :> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\(x :> xs) -> iterate (+ x) x :> (multiples <$> xs)) (1 :> unfold (\a -> (a, succ a)) (succ 1))`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) (iterate (+ 1) 1 :> (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Inline `iterate`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\f a -> unfold (\a -> (a, f a)) a) (+ 1) 1 :> (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Evaluate `(\f a -> unfold (\a -> (a, f a)) a) (+ 1)`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\a -> unfold (\a -> (a, (+ 1) a)) a) 1 :> (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Evaluate `(\a -> unfold (\a -> (a, (+ 1) a))) 1`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) (unfold (\a -> (a, (+ 1) a)) 1 :> (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Inline `unfold`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, (+ 1) a)) 1 :> (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Evaluate `(\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, (+ 1) a)`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\a -> let (b, a') = (\a -> (a, (+ 1) a)) a in b :> unfold (\a -> (a, (+ 1) a)) a') 1 :> (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Evaluate `(\a -> (a, (+ 1) a)) a`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\a -> let (b, a') = (a, (+ 1) a) in b :> unfold (\a -> (a, (+ 1) a)) a') 1 :> (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Inline `b` and `a'`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\a -> a :> unfold (\a -> (a, (+ 1) a)) ((+ 1) a)) 1 :> (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Evaluate `(\a -> a :> unfold (\a -> (a, (+ 1) a)) ((+ 1) a)) 1`
-- (\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((1 :> unfold (\a -> (a, (+ 1) a)) ((+ 1) 1)) :> (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Evaluate `(\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((1 :> unfold (\a -> (a, (+ 1) a)) ((+ 1) 1)) :> (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))`
-- 1 :> unfold (\a -> (a, (+ 1) a)) ((+ 1) 1) `union` unionAll (multiples <$> (unfold (\a -> (a, succ a)) (succ 1)))
--
-- Inline `union`
-- 1 :> (\(x :> xs) (y :> ys) -> case compare x y of LT -> x :> (xs `union` (y :> ys)); EQ -> x :> (xs `union` ys); GT -> y :> ((x :> xs) `union` ys)) (unfold (\a -> (a, (+ 1) a)) ((+ 1) 1)) (unionAll (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Inline `unfold`
-- 1 :> (\(x :> xs) (y :> ys) -> case compare x y of LT -> x :> (xs `union` (y :> ys)); EQ -> x :> (xs `union` ys); GT -> y :> ((x :> xs) `union` ys)) ((\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, (+ 1) a)) ((+ 1) 1)) (unionAll (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Evaluate `(\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, (+ 1) a))`
-- 1 :> (\(x :> xs) (y :> ys) -> case compare x y of LT -> x :> (xs `union` (y :> ys)); EQ -> x :> (xs `union` ys); GT -> y :> ((x :> xs) `union` ys)) ((\a -> let (b, a') = (\a -> (a, (+ 1) a)) a in b :> unfold (\a -> (a, (+ 1) a)) a') ((+ 1) 1)) (unionAll (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Evaluate `(\a -> (a, (+ 1) a)) a`
-- 1 :> (\(x :> xs) (y :> ys) -> case compare x y of LT -> x :> (xs `union` (y :> ys)); EQ -> x :> (xs `union` ys); GT -> y :> ((x :> xs) `union` ys)) ((\a -> let (b, a') = (a, (+ 1) a) in b :> unfold (\a -> (a, (+ 1) a)) a') ((+ 1) 1)) (unionAll (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Inline `b` and `a'`
-- 1 :> (\(x :> xs) (y :> ys) -> case compare x y of LT -> x :> (xs `union` (y :> ys)); EQ -> x :> (xs `union` ys); GT -> y :> ((x :> xs) `union` ys)) ((\a -> a :> unfold (\a -> (a, (+ 1) a)) ((+ 1) a)) ((+ 1) 1)) (unionAll (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Evaluate `(\a -> a :> unfold (\a -> (a, (+ 1) a)) ((+ 1) a)) ((+ 1) 1)`
-- 1 :> (\(x :> xs) (y :> ys) -> case compare x y of LT -> x :> (xs `union` (y :> ys)); EQ -> x :> (xs `union` ys); GT -> y :> ((x :> xs) `union` ys)) (((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) (unionAll (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Evaluate `(\(x :> xs) (y :> ys) -> case compare x y of LT -> x :> (xs `union` (y :> ys)); EQ -> x :> (xs `union` ys); GT -> y :> ((x :> xs) `union` ys)) (((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) (unionAll (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Inline `unionAll`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) (multiples <$> (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Inline `<$>`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\f (x :> xs) -> f x :> (f <$> xs)) multiples (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Evaluate `(\f (x :> xs) -> f x :> (f <$> xs)) multiples`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Inline `unfold`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) ((\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, succ a)) (succ 1))))
--
-- Evaluate `(\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, succ a))`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) ((\a -> let (b, a') = (\a -> (a, succ a)) a in b :> unfold (\a -> (a, succ a)) a') (succ 1))))
--
-- Evaluate `(\a -> (a, succ a)) a`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) ((\a -> let (b, a') = (a, succ a) in b :> unfold (\a -> (a, succ a)) a') (succ 1))))
--
-- Inline `b` and `a'`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) ((\a -> a :> unfold (\a -> (a, succ a)) (succ a)) (succ 1))))
--
-- Evaluate `(\a -> a :> unfold (\a -> (a, succ a)) (succ a)) (succ 1)`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) (succ 1 :> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\(x :> xs) -> multiples x :> (multiples <$> xs)) (succ 1 :> unfold (\a -> (a, succ a)) (succ (succ 1)))`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) (multiples (succ 1) :> (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Inline `multiples`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\n -> iterate (+ n) n) (succ 1) :> (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\n -> iterate (+ n) n) (succ 1)`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) (iterate (+ (succ 1)) (succ 1) :> (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Inline `iterate`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\f a -> unfold (\a -> (a, f a)) a) (+ (succ 1)) (succ 1) :> (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\f a -> unfold (\a -> (a, f a)) a) (+ (succ 1))`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\a -> unfold (\a -> (a, (+ (succ 1)) a)) a) (succ 1) :> (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\a -> unfold (\a -> (a, (+ (succ 1)) a)) a) (succ 1)`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) (unfold (\a -> (a, (+ (succ 1)) a)) (succ 1) :> (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Inline `unfold`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, (+ (succ 1)) a)) (succ 1) :> (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, (+ (succ 1)) a))`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\a -> let (b, a') = (\a -> (a, (+ (succ 1)) a)) a in b :> unfold (\a -> (a, (+ (succ 1)) a)) a') (succ 1) :> (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\a -> (a, (+ (succ 1)) a)) a`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\a -> let (b, a') = (a, (+ (succ 1)) a) in b :> unfold (\a -> (a, (+ (succ 1)) a)) a') (succ 1) :> (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Inline `b` and `a'`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((\a -> a :> unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) a)) (succ 1) :> (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\a -> a :> unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) a)) (succ 1)`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) ((\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((succ 1 :> unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1))) :> (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\((x :> xs) :> xss) -> x :> (xs `union` unionAll xss)) ((succ 1 :> unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1))) :> (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1))))`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) (succ 1 :> (unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` unionAll (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (y :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ys); GT -> y :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys)) (succ 1 :> (unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` unionAll (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))`
-- 1 :> case compare ((+ 1) 1) (succ 1) of LT -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` ((succ 1) :> ys)); EQ -> ((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)) `union` (unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` unionAll (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1))))); GT -> (succ 1) :> ((((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` unionAll (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(+ 1) 1` ('x')
-- 1 :> case compare 2 (succ 1) of LT -> 2 :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) 2) `union` ((succ 1) :> ys)); EQ -> 2 :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) 2) `union` (unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` unionAll (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1))))); GT -> (succ 1) :> ((2 :> unfold (\a -> (a, (+ 1) a)) ((+ 1) 2)) `union` (unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` unionAll (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(succ 1)` ('y')
-- 1 :> case compare 2 2 of LT -> 2 :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) 2) `union` (2 :> ys)); EQ -> 2 :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) 2) `union` (unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` unionAll (multiples <$> unfold (\a -> (a, succ a)) (succ 2)))); GT -> 2 :> ((2 :> unfold (\a -> (a, (+ 1) a)) ((+ 1) 2)) `union` (unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` unionAll (multiples <$> unfold (\a -> (a, succ a)) (succ 2))))
--
-- Evaluate `compare 2 2`
-- 1 :> case EQ of LT -> 2 :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) 2) `union` (2 :> ys)); EQ -> 2 :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) 2) `union` (unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` unionAll (multiples <$> unfold (\a -> (a, succ a)) (succ 2)))); GT -> 2 :> ((2 :> unfold (\a -> (a, (+ 1) a)) ((+ 1) 2)) `union` (unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` unionAll (multiples <$> unfold (\a -> (a, succ a)) (succ 2))))
--
-- Evaluate `(case EQ of LT -> 2 :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) 2) `union` (2 :> ys)); EQ -> 2 :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) 2) `union` (unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` unionAll (multiples <$> unfold (\a -> (a, succ a)) (succ 2)))); GT -> 2 :> ((2 :> unfold (\a -> (a, (+ 1) a)) ((+ 1) 2)) `union` (unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` unionAll (multiples <$> unfold (\a -> (a, succ a)) (succ 2)))))`
-- 1 :> 2 :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) 2) `union` (unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` unionAll (multiples <$> unfold (\a -> (a, succ a)) (succ 2))))
--
-- OK, I think that proves to me that the first version is productive. Now onto the version using `fold`
--
-- unionAll' :: Ord a => Stream (Stream a) -> Stream a
-- unionAll' = (fold (\(x :> xs) xss -> x :> xs `union` xss))
--
-- fold = (\f (x :> xs) -> f x (fold f xs))
-- multiples = (\n -> iterate (+ n) n)
-- iterate = (\f a -> unfold (\a -> (a, f a)) a)
-- unfold = (\f a -> let (b, a') = f a in b :> unfold f a')
--
-- Start:
-- unionAll' (multiples <$> enumFrom 1)
--
-- Inline `unionAll'`:
-- (fold (\(x :> xs) xss -> x :> xs `union` xss)) (multiples <$> enumFrom 1)
--
-- Inline `fold`
-- (\f (x :> xs) -> f x (fold f xs)) (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> enumFrom 1)
--
-- Evaluate `(\f (x :> xs) -> f x (fold f xs)) (\(x :> xs) xss -> x :> xs `union` xss)`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) (multiples <$> enumFrom 1)
--
-- Inline `<$>`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\f (x :> xs) -> f x :> (f <$> xs)) multiples (enumFrom 1))
--
-- Evaluate `(\f (x :> xs) -> f x :> (f <$> xs)) multiples`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) (enumFrom 1))
--
-- Inline `enumFrom`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) (iterate succ 1))
--
-- Inline `iterate`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) ((\f a -> unfold (\a -> (a, f a)) a) succ 1))
--
-- Evaluate `(\f a -> unfold (\a -> (a, f a)) a) succ`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) ((\a -> unfold (\a -> (a, succ a)) a) 1))
--
-- Evaluate `(\a -> unfold (\a -> (a, succ a)) a) 1`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) (unfold (\a -> (a, succ a)) 1))
--
-- Inline `unfold`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) ((\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, succ a)) 1))
--
-- Evaluate `(\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, succ a))`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) ((\a -> let (b, a') = (\a -> (a, succ a)) a in b :> unfold (\a -> (a, succ a)) a') 1))
--
-- Evaluate `(\a -> (a, succ a)) a`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) ((\a -> let (b, a') = (a, succ a) in b :> unfold (\a -> (a, succ a)) a') 1))
--
-- Inline `b` and `a'`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) ((\a -> a :> unfold (\a -> (a, succ a)) (succ a)) 1))
--
-- Evaluate `(\a -> a :> unfold (\a -> (a, succ a)) (succ a)) 1`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) (1 :> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\(x :> xs) -> multiples x :> (multiples <$> xs)) (1 :> unfold (\a -> (a, succ a)) (succ 1))`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) (multiples 1 :> (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Inline `multiples`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\n -> iterate (+ n) n) 1 :> (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\n -> iterate (+ n) n) 1`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) (iterate (+ 1) 1 :> (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Inline `iterate`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\f a -> unfold (\a -> (a, f a)) a) (+ 1) 1 :> (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\f a -> unfold (\a -> (a, f a)) a) (+ 1)`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\a -> unfold (\a -> (a, (+ 1) a)) a) 1 :> (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\a -> unfold (\a -> (a, (+ 1) a)) a) 1`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) (unfold (\a -> (a, (+ 1) a)) 1 :> (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Inline `unfold`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, (+ 1) a)) 1 :> (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, (+ 1) a))`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\a -> let (b, a') = (\a -> (a, (+ 1) a)) a in b :> unfold (\a -> (a, (+ 1) a)) a') 1 :> (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\a -> (a, (+ 1) a)) a`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\a -> let (b, a') = (a, (+ 1) a) in b :> unfold (\a -> (a, (+ 1) a)) a') 1 :> (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Inline `b` and `a'`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\a -> a :> unfold (\a -> (a, (+ 1) a)) ((+ 1) a)) 1 :> (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\a -> a :> unfold (\a -> (a, (+ 1) a)) ((+ 1) a)) 1`
-- (\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((1 :> unfold (\a -> (a, (+ 1) a)) ((+ 1) 1)) :> (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((1 :> unfold (\a -> (a, (+ 1) a)) ((+ 1) 1)) :> (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))`
-- (\(x :> xs) xss -> x :> xs `union` xss) (1 :> unfold (\a -> (a, (+ 1) a)) ((+ 1) 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\(x :> xs) xss -> x :> xs `union` xss) (1 :> unfold (\a -> (a, (+ 1) a)) ((+ 1) 1))`
-- (\xss -> 1 :> unfold (\a -> (a, (+ 1) a)) ((+ 1) 1) `union` xss) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\xss -> 1 :> unfold (\a -> (a, (+ 1) a)) ((+ 1) 1) `union` xss) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))`
-- 1 :> unfold (\a -> (a, (+ 1) a)) ((+ 1) 1) `union` fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 1))
--
-- Inline `union`
-- 1 :> (\(x :> xs) (y :> ys) -> case compare x y of LT -> x :> (xs `union` (y :> ys)); EQ -> x :> (xs `union` ys); GT -> y :> ((x :> xs) `union` ys)) (unfold (\a -> (a, (+ 1) a)) ((+ 1) 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Inline `unfold`
-- 1 :> (\(x :> xs) (y :> ys) -> case compare x y of LT -> x :> (xs `union` (y :> ys)); EQ -> x :> (xs `union` ys); GT -> y :> ((x :> xs) `union` ys)) ((\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, (+ 1) a)) ((+ 1) 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, (+ 1) a))`
-- 1 :> (\(x :> xs) (y :> ys) -> case compare x y of LT -> x :> (xs `union` (y :> ys)); EQ -> x :> (xs `union` ys); GT -> y :> ((x :> xs) `union` ys)) ((\a -> let (b, a') = (\a -> (a, (+ 1) a)) a in b :> unfold (\a -> (a, (+ 1) a)) a') ((+ 1) 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\a -> (a, (+ 1) a)) a`
-- 1 :> (\(x :> xs) (y :> ys) -> case compare x y of LT -> x :> (xs `union` (y :> ys)); EQ -> x :> (xs `union` ys); GT -> y :> ((x :> xs) `union` ys)) ((\a -> let (b, a') = (a, (+ 1) a) in b :> unfold (\a -> (a, (+ 1) a)) a') ((+ 1) 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Inline `b` and `a'`
-- 1 :> (\(x :> xs) (y :> ys) -> case compare x y of LT -> x :> (xs `union` (y :> ys)); EQ -> x :> (xs `union` ys); GT -> y :> ((x :> xs) `union` ys)) ((\a -> a :> unfold (\a -> (a, (+ 1) a)) ((+ 1) a)) ((+ 1) 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\a -> a :> unfold (\a -> (a, (+ 1) a)) ((+ 1) a)) ((+ 1) 1)`
-- 1 :> (\(x :> xs) (y :> ys) -> case compare x y of LT -> x :> (xs `union` (y :> ys)); EQ -> x :> (xs `union` ys); GT -> y :> ((x :> xs) `union` ys)) (((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\(x :> xs) (y :> ys) -> case compare x y of LT -> x :> (xs `union` (y :> ys)); EQ -> x :> (xs `union` ys); GT -> y :> ((x :> xs) `union` ys)) (((+ 1) 1) :> unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Inline `fold`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\f (x :> xs) -> f x (fold f xs)) (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Evaluate `(\f (x :> xs) -> f x (fold f xs)) (\(x :> xs) xss -> x :> xs `union` xss)`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) (multiples <$> unfold (\a -> (a, succ a)) (succ 1)))
--
-- Inline `<$>`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\f (x :> xs) -> f x :> (f <$> xs)) multiples (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Evaluate `(\f (x :> xs) -> f x :> (f <$> xs)) multiples`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) (unfold (\a -> (a, succ a)) (succ 1))))
--
-- Inline `unfold`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) ((\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, succ a)) (succ 1))))
--
-- Evaluate `(\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, succ a))`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) ((\a -> let (b, a') = (\a -> (a, succ a)) a in b :> unfold (\a -> (a, succ a)) a') (succ 1))))
--
-- Evaluate `(\a -> (a, succ a)) a`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) ((\a -> let (b, a') = (a, succ a) in b :> unfold (\a -> (a, succ a)) a') (succ 1))))
--
-- Inline `b` and `a'`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) ((\a -> a :> unfold (\a -> (a, succ a)) (succ a)) (succ 1))))
--
-- Evaluate `(\a -> a :> unfold (\a -> (a, succ a)) (succ a)) (succ 1)`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) ((\(x :> xs) -> multiples x :> (multiples <$> xs)) (succ 1 :> unfold (\a -> (a, succ a)) (succ (succ 1))))
--
-- Evaluate `(\(x :> xs) -> multiples x :> (multiples <$> xs)) (succ 1 :> unfold (\a -> (a, succ a)) (succ (succ 1)))`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) (multiples (succ 1) :> (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\(x :> xs) -> (\(x :> xs) xss -> x :> xs `union` xss) x (fold (\(x :> xs) xss -> x :> xs `union` xss) xs)) (multiples (succ 1) :> (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1))))`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) xss -> x :> xs `union` xss) (multiples (succ 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Inline `multiples`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) xss -> x :> xs `union` xss) ((\n -> iterate (+ n) n) (succ 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\n -> iterate (+ n) n) (succ 1)`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) xss -> x :> xs `union` xss) (iterate (+ (succ 1)) (succ 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Inline `iterate`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) xss -> x :> xs `union` xss) ((\f a -> unfold (\a -> (a, f a)) a) (+ (succ 1)) (succ 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\f a -> unfold (\a -> (a, f a)) a) (+ (succ 1))`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) xss -> x :> xs `union` xss) ((\a -> unfold (\a -> (a, (+ (succ 1)) a)) a) (succ 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\a -> unfold (\a -> (a, (+ (succ 1)) a)) a) (succ 1)`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) xss -> x :> xs `union` xss) (unfold (\a -> (a, (+ (succ 1)) a)) (succ 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Inline `unfold`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) xss -> x :> xs `union` xss) ((\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, (+ (succ 1)) a)) (succ 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\f a -> let (b, a') = f a in b :> unfold f a') (\a -> (a, (+ (succ 1)) a))`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) xss -> x :> xs `union` xss) ((\a -> let (b, a') = (\a -> (a, (+ (succ 1)) a)) a in b :> unfold (\a -> (a, (+ (succ 1)) a)) a') (succ 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\a -> (a, (+ (succ 1)) a)) a`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) xss -> x :> xs `union` xss) ((\a -> let (b, a') = (a, (+ (succ 1)) a) in b :> unfold (\a -> (a, (+ (succ 1)) a)) a') (succ 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Inline `b` and `a'`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) xss -> x :> xs `union` xss) ((\a -> a :> unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) a)) (succ 1)) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\a -> a :> unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) a)) (succ 1)`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\(x :> xs) xss -> x :> xs `union` xss) (succ 1 :> unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1))) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\(x :> xs) xss -> x :> xs `union` xss) (succ 1 :> unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)))`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) ((\xss -> succ 1 :> unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` xss) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\xss -> succ 1 :> unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` xss) (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1))))`
-- 1 :> (\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) (succ 1 :> unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))
--
-- Evaluate `(\(y :> ys) -> case compare ((+ 1) 1) y of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` (y :> ys)); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ys); GT -> y :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` ys)) (succ 1 :> unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))`
-- 1 :> (case compare ((+ 1) 1) (succ 1) of LT -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` ((succ 1) :> unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))); EQ -> ((+ 1) 1) :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1))) `union` unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1))))); GT -> (succ 1) :> ((((+ 1) 1) :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) ((+ 1) 1)))) `union` unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1))))))
--
-- Evaluate `((+ 1) 1)` 'x'
-- 1 :> (case compare 2 (succ 1) of LT -> 2 :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) 2)) `union` ((succ 1) :> unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1)))))); EQ -> 2 :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) 2)) `union` unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1))))); GT -> (succ 1) :> ((2 :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) 2))) `union` unfold (\a -> (a, (+ (succ 1)) a)) ((+ (succ 1)) (succ 1)) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ (succ 1))))))
--
-- Evaluate `(succ 1)` 'y'
-- 1 :> (case compare 2 2 of LT -> 2 :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) 2)) `union` (2 :> unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 2))))); EQ -> 2 :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) 2)) `union` unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 2)))); GT -> 2 :> ((2 :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) 2))) `union` unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 2)))))
--
-- Evaluate `compare 2 2`
-- 1 :> (case EQ of LT -> 2 :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) 2)) `union` (2 :> unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 2))))); EQ -> 2 :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) 2)) `union` unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 2)))); GT -> 2 :> ((2 :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) 2))) `union` unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 2)))))
--
-- Evaluate `(case EQ of LT -> 2 :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) 2)) `union` (2 :> unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 2))))); EQ -> 2 :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) 2)) `union` unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 2)))); GT -> 2 :> ((2 :> (unfold (\a -> (a, (+ 1) a)) ((+ 1) 2))) `union` unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 2)))))`
-- 1 :> 2 :> ((unfold (\a -> (a, (+ 1) a)) ((+ 1) 2)) `union` unfold (\a -> (a, (+ 2) a)) ((+ 2) 2) `union` (fold (\(x :> xs) xss -> x :> xs `union` xss) (multiples <$> unfold (\a -> (a, succ a)) (succ 2))))
--
-- Hrm ... that seems productive also, I must have made some kind of mistake.

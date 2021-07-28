{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (find, maximumBy, unfoldr)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Numeric.Natural (Natural)
import Digits (digits)

splits :: forall a. [a] -> [(Seq a, [a])]
splits = unfoldr alg . first Seq.fromList . splitAt 0
  where
    alg :: (Seq a, [a]) -> Maybe ((Seq a, [a]), (Seq a, [a]))
    alg (_, []) = Nothing
    alg (p, x : xs) = let p' = p |> x in Just ((p', xs), (p', xs))

recurrance :: Eq a => [a] -> Maybe (Seq a)
recurrance =
  (fst <$>)
    . find (\(p, xs) -> toList p == take (length p) xs)
    . splits

solve :: Natural -> Natural
solve n =
  maximumBy
    ( on
        compare
        (fmap length . recurrance . digits . div (10 ^ (n * 2)))
    )
    [1 .. n]

main :: IO ()
main = print (solve 1000)

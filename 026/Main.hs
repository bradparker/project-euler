{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

-- ~ $ ghc -O2 ./Main.hs
-- ~ $ $(which time) -v ./Main
-- 983
--         Command being timed: "./Main"
--         User time (seconds): 0.38
--         System time (seconds): 0.01
--         Percent of CPU this job got: 96%
--         Elapsed (wall clock) time (h:mm:ss or m:ss): 0:00.41
--         Average shared text size (kbytes): 0
--         Average unshared data size (kbytes): 0
--         Average stack size (kbytes): 0
--         Average total size (kbytes): 0
--         Maximum resident set size (kbytes): 4752
--         Average resident set size (kbytes): 0
--         Major (requiring I/O) page faults: 0
--         Minor (reclaiming a frame) page faults: 1318
--         Voluntary context switches: 0
--         Involuntary context switches: 77
--         Swaps: 0
--         File system inputs: 0
--         File system outputs: 0
--         Socket messages sent: 0
--         Socket messages received: 0
--         Signals delivered: 0
--         Page size (bytes): 4096
--         Exit status: 0

module Main where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Function (on, (&))
import Data.List (find, maximumBy, tails, unfoldr)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ratio ((%))
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Word (Word8)

splits :: forall a. [a] -> [(Seq a, [a])]
splits = unfoldr alg . first Seq.fromList . splitAt 0
  where
    alg :: (Seq a, [a]) -> Maybe ((Seq a, [a]), (Seq a, [a]))
    alg (_, []) = Nothing
    alg (p, x : xs) = let p' = p |> x in Just ((p', xs), (p', xs))

recurrance :: forall a. Eq a => [a] -> Maybe (Seq a)
recurrance =
  (fst <$>)
    . find (\(p, xs) -> toList p == take (Seq.length p) xs)
    . splits

decimalDigits :: forall a b. (Integral a, RealFrac b) => b -> [a]
decimalDigits =
  unfoldr $ \n ->
    if n == 0
      then Nothing
      else
        let m = floor (n * 10)
         in Just (m, n * 10 - fromIntegral m)

repetend :: forall a b. (Integral a, RealFrac b) => b -> Maybe (Seq a)
repetend n =
  n
    & decimalDigits
    & dropWhile (== 0)
    & tails
    & mapMaybe (recurrance . take (ceiling (recip n) * 2))
    & listToMaybe

period :: forall b. RealFrac b => b -> Int
period = maybe 0 length . repetend @Word8

largestRepetend :: Int -> Int
largestRepetend = maximumBy (on compare (period . (1 %))) . enumFromTo 1

main :: IO ()
main = print (largestRepetend 1000)

{-# OPTIONS_GHC -Wall #-}

module Main where

-- Using names.txt (right click and 'Save Link/Target As...'), a 46K
-- text file containing over five-thousand first names, begin by
-- sorting it into alphabetical order. Then working out the
-- alphabetical value for each name, multiply this value by its
-- alphabetical position in the list to obtain a name score.

-- For example, when the list is sorted into alphabetical order, COLIN,
-- which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the
-- list. So, COLIN would obtain a score of 938 × 53 = 49714.

-- What is the total of all the name scores in the file?

import Data.Char (ord)
import Data.List (sort)

solve :: [String] -> Int
solve = sum . zipWith (*) [1 ..] . map score . sort
  where
    score :: String -> Int
    score = sum . map (subtract 64 . ord)

parseInput :: String -> [String]
parseInput = words . map (\c -> if c == ',' then ' ' else c) . filter (/= '"')

main :: IO ()
main = pure ()

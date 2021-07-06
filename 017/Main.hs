{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Applicative ((<|>))
import Data.Char (isAlpha)
import qualified Data.Map as Map
import Numeric.Natural (Natural)

ones :: Natural -> Maybe String
ones n =
  let m =
        Map.fromList
          ( zip
              [1 ..]
              [ "one",
                "two",
                "three",
                "four",
                "five",
                "six",
                "seven",
                "eight",
                "nine"
              ]
          )
   in Map.lookup n m

teens :: Natural -> Maybe String
teens n =
  let m =
        Map.fromList
          [ (11, "eleven"),
            (12, "twelve"),
            (13, "thirteen"),
            (15, "fifteen"),
            (18, "eighteen")
          ]
   in Map.lookup n m <|> ((<> "teen") <$> ones (n `mod` 10))

tens :: Natural -> Maybe String
tens n
  | n < 10 = ones n
  | n == 10 = Just "ten"
  | n < 20 = teens n
  | otherwise =
    let m =
          Map.fromList
            [ (2, "twenty"),
              (3, "thirty"),
              (4, "forty"),
              (5, "fifty"),
              (6, "sixty"),
              (7, "seventy"),
              (8, "eighty"),
              (9, "ninety")
            ]
     in case divMod n 10 of
          (t, 0) -> Map.lookup t m
          (t, o) -> (\t' o' -> t' <> "-" <> o') <$> Map.lookup t m <*> ones o

hundreds :: Natural -> Maybe String
hundreds n
  | n < 100 = tens n
  | otherwise = case divMod n 100 of
    (h, t) -> do
      h' <- (<> " hundred") <$> ones h
      if t == 0
        then pure h'
        else do
          t' <- tens t
          pure (h' <> " and " <> t')

main :: IO ()
main = print (length . ("onethousand" <>) . filter isAlpha <$> foldMap hundreds [1 .. 999])

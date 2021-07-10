{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

-- You are given the following information, but you may prefer to do
-- some research for yourself.
--
--     * 1 Jan 1900 was a Monday.
--     * Thirty days has September,
--     * April, June and November.
--     * All the rest have thirty-one,
--     * Saving February alone,
--     * Which has twenty-eight, rain or shine.
--     * And on leap years, twenty-nine.
--     * A leap year occurs on any year evenly divisible by 4, but not on
--       a century unless it is divisible by 400.

-- How many Sundays fell on the first of the month during the twentieth
-- century (1 Jan 1901 to 31 Dec 2000)?

import Control.Monad.State (evalState, state)
import Data.Function ((&))
import Numeric.Natural (Natural)

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Enum, Eq, Show)

thirty :: [Natural]
thirty = [1 .. 30]

thirtyOne :: [Natural]
thirtyOne = [1 .. 31]

april, june, september, november :: [Natural]
april = thirty
june = thirty
september = thirty
november = thirty

january, march, may, july, august, october, decemember :: [Natural]
january = thirtyOne
march = thirtyOne
may = thirtyOne
july = thirtyOne
august = thirtyOne
october = thirtyOne
decemember = thirtyOne

feburary :: [Natural]
feburary = [1 .. 28]

feburaryLeap :: [Natural]
feburaryLeap = [1 .. 29]

year :: [[Natural]]
year =
  [ january,
    feburary,
    march,
    april,
    may,
    june,
    july,
    august,
    september,
    october,
    november,
    decemember
  ]

leapYear :: [[Natural]]
leapYear =
  [ january,
    feburaryLeap,
    march,
    april,
    may,
    june,
    july,
    august,
    september,
    october,
    november,
    decemember
  ]

years :: [[[Natural]]]
years =
  map
    ( \n ->
        if (n `mod` 4 == 0 && n `mod` 100 /= 0) || n `mod` 400 == 0
          then leapYear
          else year
    )
    ([1900 .. 2000] :: [Natural])

overlay :: [a] -> [[b]] -> [[(b, a)]]
overlay xs =
  flip evalState xs . traverse \ys ->
    zip ys <$> state (splitAt (length ys))

occurances :: (Natural, Weekday) -> Int
occurances d =
  mconcat years
    & overlay (cycle [Monday ..])
    & drop 12
    & mconcat
    & filter (== d)
    & length

main :: IO ()
main = do
  print (occurances (1, Sunday))
  print (map occurances ((,) <$> [1 .. 31] <*> [Monday ..]))

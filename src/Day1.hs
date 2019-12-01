module Day1
    ( 
        day1
       ,day1b
    ) where

import Data.List

day1 :: String -> Int
day1 input = sum $ map (calculateMass.read) $ lines input

calculateMass :: Int -> Int
calculateMass input = floor (fromIntegral input/3)-2

input = "12"

day1b :: String -> Int
day1b input = sum $ map (day1b'.read) $ lines input 
  where day1b' input' = sum $ drop 1 $ takeWhile (>0) $ iterate calculateMass input'

module Day4
    ( 
        day4
       ,day4b
       ,rule1
       ,rule2
       ,rule3
    ) where

import Data.List.Split (splitOn)
import Data.List  (group, sort)
import qualified Data.Set as Set

input =  "382345-843167"
parseInput :: String -> (Int,Int)
parseInput input = (low,high)
    where [low,high] = map read $ splitOn "-" input

day4 :: String -> Int
day4 input = length $ [ x | x <- [low..high], rules x]
    where (low,high) = parseInput input
          rules x = rule1 x && rule2 x

day4b :: String -> Int
day4b input = length $ [ x | x <- [low..high], rules x]
    where (low,high) = parseInput input
          rules x = rule1 x && rule2 x && rule3 x

-- Two adjacent digits are the same (like 22 in 122345).
rule1 :: Int -> Bool
rule1 x = (<) 0 $ length $ filter (>1)$ map length $ group $ show x

-- Going from left to right, the digits never decrease; they only ever increase
rule2 :: Int -> Bool
rule2 x = x == read (sort $ show x)

-- the two adjacent matching digits are not part of a larger group of matching digits.
rule3 :: Int -> Bool 
rule3 x = (<) 0 $ length $ filter (<3) $ filter (>1)$ map length $ group $ show x


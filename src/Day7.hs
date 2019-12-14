module Day7
    ( 
    day7
   ,day7b
    )
    where

import Day5
import Data.Char (digitToInt,intToDigit)
import Data.List.Split (splitOn,chunksOf)
import Data.List (permutations)
    
day7' :: String -> String -> Int
day7' input input2 =   foldr(\y acc -> snd 
                                      $ intCode 0 (parseInput input) [y,acc] 0) 0
                      $ map digitToInt input2

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," input
    
input = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
input2 = "43210"

day7 :: String -> Int 
day7 input = maximum $  map (\y -> day7' input y) $ permutations "01234"

day7b :: String -> Int 
day7b input = 0



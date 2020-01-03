module Day5
    ( 
        day5
       ,day5b
       ,intCode
       ,intCode'
       ,setVal
    ) where

import Data.List.Split (splitOn)

day5 :: String -> [Int] -> Int
day5 input seed = snd $ intCode (parseInput input) seed 

day5b :: String -> [Int] -> Int
day5b input seed = snd $ intCode (parseInput input) seed

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," input

intCode :: [Int] -> [Int] -> ([Int], Int)
intCode inst input = intCode' 0 inst input 0

intCode' :: Int -> [Int] -> [Int] -> Int -> ([Int], Int)
intCode' offset inst input output
  | currInst == 99 = (inst, output)
  | currInst == 3 = intCode' (offset+2) (setVal inst pos1 currInput) inputRest output
  | currInst == 4 = intCode' (offset+2) inst input pos1Val
  | currInst == 1 = intCode' (offset+4) (setVal inst dest (pos1Val + pos2Val)) input output
  | currInst == 2 = intCode' (offset+4) (setVal inst dest (pos1Val * pos2Val)) input output
  | currInst == 5 = intCode' (if pos1Val/=0 then pos2Val else offset+3) inst input output
  | currInst == 6 = intCode' (if pos1Val==0 then pos2Val else offset+3) inst input output
  | currInst == 7 = intCode' (offset+4) (setVal inst dest (if pos1Val<pos2Val then 1 else 0)) input output
  | currInst == 8 = intCode' (offset+4) (setVal inst dest (if pos1Val==pos2Val then 1 else 0)) input output
  | currInst == 0 = intCode' (offset+pos1Val) inst input output
  | otherwise = error ("invalid opcode curr=" ++ show curr ++ " - offset=" ++ show offset ++ " : " ++ show inst)
     where curr = inst!!offset
           currInst=curr `mod` 100
           opCode = inst!!offset
           pos1 = inst!!(offset+1)
           pos2 = inst!!(offset+2)
           dest = inst!!(offset+3)
           pos1Mode = (opCode `div` 100) `mod` 10
           pos2Mode = (opCode `div` 1000) `mod` 10
           pos1Val = if pos1Mode==0 then inst!!pos1 else pos1
           pos2Val = if pos2Mode==0 then inst!!pos2 else pos2
           (currInput:inputRest) = input

setVal :: [a] -> Int -> a -> [a]
setVal lst idx val = begin ++ (val:end)
  where (begin, _:end) = splitAt idx lst 


_input="1002,4,3,4,33"

module Day7
    ( 
    day7
   ,day7b
   ,connect
    )
    where

import Day5
import Data.Char (digitToInt)
import Data.List.Split (splitOn)
import Data.List (permutations)
import Data.Either()
    
day7' :: String -> String -> Int
day7' programStr phaseStr =   foldr(\y acc -> snd 
                                      $ intCode' 0 program [y,acc] 0) 0 phase
  where program = parseInput programStr
        phase = map digitToInt phaseStr

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," input
    
_input = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
_input2 = "43210"
_input3 = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
_input4 = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"

day7 :: String -> Int 
day7 input = maximum $  map (day7' input) $ permutations "01234"

day7b :: String -> Int 
day7b input = maximum $ map(\y-> head $ go $ amplifiers y (parseInput input)) $ permutations [9,8,7,6,5]

amplifiers :: [Int] -> [Int] -> [(Int,[Int],[Int],[Int])]
amplifiers [p1,p2,p3,p4,p5] inst = 
      [ (0, inst, [p1,0], []), 
        (0, inst, [p2], []), 
        (0, inst, [p3], []), 
        (0, inst, [p4], []),
        (0, inst, [p5], []) ]

intCode'' :: (Int, [Int], [Int], [Int]) -> Either [Int] (Int, [Int], [Int], [Int])
intCode'' (offset, inst, input, output)
  | currInst == 99 = Left output
  | currInst == 3 && null input = Right (offset, inst, input, output) -- wait
  | currInst == 3 = Right (offset+2,setVal inst pos1 currInput,inputRest,output)
  | currInst == 4 = Right (offset+2,inst,input,pos1Val:output)
  | currInst == 1 = Right (offset+4,setVal inst dest (pos1Val + pos2Val),input,output)
  | currInst == 2 = Right (offset+4,setVal inst dest (pos1Val * pos2Val),input,output)
  | currInst == 5 = Right (if pos1Val/=0 then pos2Val else offset+3,inst,input,output)
  | currInst == 6 = Right (if pos1Val==0 then pos2Val else offset+3,inst,input,output)
  | currInst == 7 = Right (offset+4,setVal inst dest (if pos1Val<pos2Val then 1 else 0),input,output)
  | currInst == 8 = Right (offset+4,setVal inst dest (if pos1Val==pos2Val then 1 else 0),input,output)
  | currInst == 0 = Right (offset+pos1Val,inst,input,output)
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

runSequence (x:xs) = case intCode'' x of
    Left payload-> Left payload
    Right (offset,inst,input,output)-> Right (connect output xs
                                                      ++[(offset,inst,input,[])])

connect :: [a1] -> [(a2, b, [a1], d)] -> [(a2, b, [a1], d)]
connect output ((offset2, inst2, input2, output2):rest) 
    = (offset2, inst2, input2++output, output2):rest


go :: [(Int, [Int], [Int], [Int])] -> [Int]
go [x] = repeat' x
  where 
    repeat' x = case intCode'' x of
        Left payload -> payload
        Right payload -> repeat' payload
go (x:xs)= case runSequence (x:xs) of
  Left payload -> go xs
  Right payload -> go payload

module Day9
    ( 
    day9
   --,day9b  part b is just different input
    )
    where
    
import Data.List.Split (splitOn) 

setVal :: [Int] -> Int -> Int -> [Int]
setVal lst idx val = begin ++ (val:end)
  where (begin, _:end) = splitAt idx fixedList 
        fixedList = if idx < length lst then lst 
                    else take (idx+1) $ lst ++ repeat 0

_input="109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," input

day9 :: String -> [Int] -> [Int]
day9 program input = repeat' (0,parseInput program, input,[],0)

intCode'' :: (Int, [Int], [Int], [Int], Int) -> Either [Int] (Int, [Int], [Int], [Int], Int)
intCode'' (offset, inst, input, output, relBase)
  | currInst == 99 = Left output
  | currInst == 3 && null input = Right (offset, inst, input, output, relBase) -- wait
  | currInst == 3 = Right (offset+2,setVal inst pos1Literal currInput,inputRest,output, relBase)
  | currInst == 4 = Right (offset+2,inst,input,output++[pos1Val], relBase)
  | currInst == 1 = Right (offset+4,setVal inst dest (pos1Val + pos2Val),input,output,relBase)
  | currInst == 2 = Right (offset+4,setVal inst dest (pos1Val * pos2Val),input,output,relBase)
  | currInst == 5 = Right (if pos1Val/=0 then pos2Val else offset+3,inst,input,output,relBase)
  | currInst == 6 = Right (if pos1Val==0 then pos2Val else offset+3,inst,input,output,relBase)
  | currInst == 7 = Right (offset+4,setVal inst dest (if pos1Val<pos2Val then 1 else 0),input,output,relBase)
  | currInst == 8 = Right (offset+4,setVal inst dest (if pos1Val==pos2Val then 1 else 0),input,output,relBase)
  | currInst == 9 = Right (offset+2,inst,input,output,relBase+pos1Val)
  | otherwise = error ("invalid opcode curr=" ++ show curr ++ " - offset=" ++ show offset ++ " : " ++ show inst)
     where curr = inst!!offset
           currInst=curr `mod` 100
           opCode = inst!!offset
           pos1 = inst!!(offset+1)
           pos2 = inst!!(offset+2)
           pos1Mode = (opCode `div` 100) `mod` 10
           pos2Mode = (opCode `div` 1000) `mod` 10
           pos3Mode = (opCode `div` 10000) `mod` 10
           dest = case pos3Mode of 
                2 -> inst!!(offset+3) + relBase
                _ -> inst!!(offset+3)
           pos1Literal = case pos1Mode of
                     2 -> relBase+pos1
                     _ -> pos1 
           pos1Val = case pos1Mode of
                     0 -> safeGet inst pos1 
                     1 -> pos1
                     2 -> safeGet inst (relBase+pos1)
           pos2Val = case pos2Mode of
                     0 -> safeGet inst pos2 
                     1 -> pos2
                     2 -> safeGet inst (relBase+pos2)
           (currInput:inputRest) = input
           safeGet set offset = if offset > length set then 0 else set!!offset

repeat' :: (Int, [Int],[Int],[Int],Int) -> [Int]
repeat' x = case intCode'' x of
    Left payload -> payload
    Right payload -> repeat' payload

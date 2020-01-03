module Day2
    ( 
        day2
       ,day2b
       ,intCode
    ) where
import Data.List.Split (splitOn)

_input="1,9,10,3,2,3,11,0,99,30,40,50"

day2 :: String -> Int
day2 input = head $ intCode 0 $ seedInput 12 2 $ parseInput input

day2b input = length $ takeWhile (/=19690720) 
        $ map (\(a,b) -> head $ intCode 0 (seedInput a b parsedInput)) [(x,y) | x<-[0..99], y<-[0..99]]
  where parsedInput = parseInput input

seedInput :: Int -> Int -> [Int] -> [Int]
seedInput x y parsedInput = setVal (setVal parsedInput 1 x) 2 y

intCode :: Int -> [Int] -> [Int]
intCode offset inst 
  | inst!! offset == 99 = inst
  | inst!! offset == 1 = intCode (offset+4) (setVal inst dest (inst!!pos1 + inst!!pos2))
  | inst!! offset == 2 = intCode (offset+4) (setVal inst dest (inst!!pos1 * inst!!pos2))
  | otherwise = error ("invalid opcode " ++ show offset ++ " : " ++ show inst)
    where [_,pos1,pos2,dest] = take 4 $ drop offset inst

setVal :: [a] -> Int -> a -> [a]
setVal lst idx val = begin ++ (val:end)
  where (begin, _:end) = splitAt idx lst 

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," input

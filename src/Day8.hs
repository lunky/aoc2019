module Day8
    ( 
    day8
   ,day8b
    )
    where
import Data.Function (on)
import Data.List (minimumBy, transpose)
import Data.List.Split (chunksOf)
    
day8 :: (Int,Int) -> String -> Int 
day8 (width,height) input = (\y -> countLetters y '2' * countLetters y '1') 
                    $ leastZeros 
                    $ chunksOf (width*height) 
                    $ filter (/='\n') input

leastZeros = minimumBy (compare `on` zeros) 
  where zeros input = countLetters input '0'

countLetters :: String -> Char -> Int
countLetters str c = length $ filter (==c) str

day8b :: (Int,Int) -> String -> String
day8b (width,height) input= unlines $ chunksOf width 
                              $ day8b' input (width, height)

helpShow '1' = '#'
helpShow '0' = ' '
helpShow x = x

day8b' input (width,height) = map (helpShow.color) $ transpose $ chunksOf (width*height) $ filter (/='\n') input

_input = "0222112222120000"

color = foldr (\y acc-> if y/='2' then y else acc) '2'

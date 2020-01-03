module Day3
    ( 
        day3
       ,day3b
       ,move
    ) where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set

--_input =  "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
_input =  "R8,U5,L5,D3\nU7,R6,D4,L4"

parseInput :: String -> [[(Char, Int)]]
parseInput input = map (map (\(x:xs)->(x, read xs)) . splitOn ",") $ lines input


day3 :: String -> Int
day3 input = minimum $ map (\(x,y)->abs x+ abs y) $ day3'' input

day3'' :: String -> [(Int,Int)]
day3'' input = filter (/=(0,0)) $ setIntersect a b
  where (a,b) = (\y -> (head y, y!!1)) 
          $ map (fst . foldr move' ([],(0,0)) . reverse)
          $ parseInput  input

setIntersect :: Ord b => [b] -> [b] -> [b] -- 6.5
setIntersect a b = Set.toList $ Set.intersection (Set.fromList a) (Set.fromList b)

day3b :: String -> Int
day3b input = minimum $ map (\(k,v) -> (aMap Map.! k) + (bMap Map.! k) ) $ Map.toList $ Map.intersection aMap bMap
  where 
        aMap = Map.fromList (zip a [1..])
        bMap = Map.fromList (zip b [1..])
        (a,b) = (\y ->(tail $ head y, tail (y!!1)))
              $ map (fst . foldr move' ([(0,0)],(0,0)) . reverse)
              $ parseInput input 

move' (dir,dist) (grid,(curX,curY)) = (grid++d, last d)
  where d = move (dir,dist) (grid,(curX,curY))

move (dir,dist) (grid,(curX,curY))
  | dir=='R' = tail [ (x,curY) | x<- [curX..(curX+dist)]]
  | dir=='L' = tail [ (x,curY) | x<- [curX,curX-1..(curX-dist)]]
  | dir=='U' = tail [ (curX,y) | y<- [curY..curY+dist]]
  | dir=='D' = tail [ (curX,y) | y<- [curY,curY-1..(curY-dist)]]


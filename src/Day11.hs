module Day11
    ( 
    day11
   ,day11b
   ,move'
   ,Direction(..)
   ,Orientation(..)
   ,Coord(..)
   ,Move(..)
    )
    where

import Data.List.Split (splitOn) 
import Day9 (intCode)
import Data.Map (Map)
import qualified Data.Map as Map

data Orientation = North | East | South | West deriving (Show,Enum,Eq)
data Direction = Clockwise | CounterClockwise deriving (Show,Eq)

day11 :: String -> Int 
day11 input =  length input

--day11' :: String -> [Int]
_day11' program input = compute (IntCode (0,(parseInput program), [input],[],0))

data Coord = Coord (Int,Int) deriving (Show, Eq, Ord)
data IntCode  = IntCode (Int, Map Int Int, [Int], [Int],Int) deriving (Show)
data Move = Move (Coord, Orientation, Map Coord Int) deriving (Show,Eq)

compute :: IntCode -> (Int,Bool)
compute (IntCode x) = case intCode x of
    Left payload -> (head payload, True)
    Right (offset, inst, input, output, relBase ) -> 
        if output == [] then 
          compute (IntCode (offset, inst, input, output, relBase))
        else (head output, False)


turn :: Direction -> Orientation -> Orientation 
turn Clockwise West = North
turn CounterClockwise North = West 
turn newDir curr = case (newDir) of 
                     Clockwise -> succ curr
                     CounterClockwise -> pred curr

move' :: Int -> Move -> Move
move' direction details =  move directionTranslated details
      where directionTranslated = if direction == 1 then Clockwise 
                                  else CounterClockwise


move :: Direction -> Move -> Move
move direction (Move(Coord(currX,currY),currOrientation,grid)) = 
                                  Move (newLoc, newOrientation, nextGrid)
  where nextGrid =  Map.insertWith (\old new -> if new == 0 then 1 else 0) (Coord(currX,currY)) 0 grid
        newOrientation = turn direction currOrientation
        newLoc = case (newOrientation) of 
                    North -> Coord(currX,currY+1)
                    East  -> Coord(currX+1,currY)
                    South -> Coord(currX,currY-1)
                    West  -> Coord(currX-1,currY)

day11b :: String -> Int
day11b input = 0

parseInput :: String -> Map Int Int
parseInput input = Map.fromList $ zip [0..] $ map read $ splitOn "," input

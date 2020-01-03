module Day10
    ( 
    day10
   ,day10b
   ,canSee
   ,collinear
   ,parseInput
    )
    where
    
import Data.Function (on)
import Data.List (maximumBy)

day10 :: String -> ((Int,Int),Int)
day10 input = maximumBy (compare `on` snd) $ day10' input

day10' input = (\set -> map (\y -> (y,length $ canSee y set) ) set) 
                  $ parseInput input

day10b :: String -> Int
day10b input = 0

input = ".#..#\n.....\n#####\n....#\n...##\n"
input2 = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"

lineOfSite :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> Bool
lineOfSite p1 p2 set = all (==False) 
           $ map (\p3 -> collinear p1 p2 p3 
                   && (distance p1 p2 > distance p1 p3) 
                   && (distance p1 p2 > distance p2 p3) 
--                   && ((distance p1 p3 + distance p2 p3 == distance p1 p2)
                 ) rest 
  where rest = filter (\y -> y/=p1 && y/=p2) set

canSee :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)] 
canSee p1 set = filter (\y -> lineOfSite p1 y set) $ filter (/=p1) set

collinear :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
collinear (x1,y1) (x2,y2) (x3,y3)= (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1) == 0

distance :: (Real a, Floating b) => (a,a) -> (a,a) -> b
distance (x1,y1) (x2,y2) = sqrt (realToFrac ((x1 - x2)^2 + (y1 - y2)^2))


parseInput :: String -> [(Int,Int)]
parseInput input =  map fst
                    $ filter(\(coord,datum)->datum /='.') 
                    $ concatMap (\(y,row) -> map (\(x,datum)-> ((x,y),datum) )  row) 
                    $ zip [0..] $ map (zip [0..]) 
                    $ lines input

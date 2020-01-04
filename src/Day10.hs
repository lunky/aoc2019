{-# OPTIONS_GHC -Wno-type-defaults #-}
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
import Data.List (maximumBy,sortBy)

day10 :: String -> Int
day10 input = snd $ maximumBy (compare `on` snd) $ day10' input

day10' :: String -> [((Int, Int), Int)]
day10' input = (\set -> map (\y -> (y,length $ canSee y set) ) set) 
                  $ parseInput input

day10b :: String -> Int -> Int
day10b input offset = (\(x,y) -> (x * 100) + y) $ day10b' center (parseInput input)!!offset
    where center = fst $ maximumBy (compare `on` snd) $ day10' input
          day10b' center' parsedInput = sortBy (clockwise center')  
                    $ canSee center parsedInput

clockwise :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Ordering
clockwise (cx,cy) (ax,ay) (bx,by)
    | ax - cx >= 0 && bx - cx < 0 =  LT   -- a is left of center
    | ax - cx < 0 && bx - cx >= 0 = GT    -- a is right of center
    | (ax - cx == 0 && bx - cx == 0)      -- both are on horizontal center 
                && (ay - cy >= 0 || by - cy >= 0)  = compare ay by
    | ax - cx == 0 && bx - cx == 0                 = compare by ay
--   compute the cross product of vectors (center -> a) x (center -> b)
    | det > 0 = LT -- a is clockwise first 
    | det < 0 = GT -- b is clockwise first
    -- points a and b are on the same line from the center
    -- check which point is closer to the center
    | otherwise = compare ((ax - cx) * (ax - cx) + (ay - cy) * (ay - cy))
                          ((bx - cx) * (bx - cx) + (by - cy) * (by - cy))
      where det=(ax - cx) * (by - cy) - (bx - cx) * (ay - cy)

_input :: String
_input = ".#..#\n.....\n#####\n....#\n...##\n"

_input2 :: String
_input2 = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"

_input3 :: String
_input3=".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....X...###..\n..#.#.....#....##"

_input4 :: String
_input4=".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##\n"

lineOfSite :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> Bool
lineOfSite p1 p2 set = all (==False) 
           $ map (\p3 -> collinear p1 p2 p3 
                   && (distance p1 p2 > distance p1 p3) 
                   && (distance p1 p2 > distance p2 p3) 
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
                    $ filter(\(_,datum)->datum /='.') 
                    $ concatMap (\(y,row) -> map (\(x,datum)-> ((x,y),datum) )  row) 
                    $ zip [0..] $ map (zip [0..]) 
                    $ lines input

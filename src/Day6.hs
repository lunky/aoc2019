module Day6
    ( 
        day6
       ,day6b
    ) where

import Data.List.Split (splitOn)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.Foldable (find)
import Data.Maybe (catMaybes, mapMaybe, isJust)
import Control.Monad (join)

firstJust :: [Maybe a] -> Maybe a
firstJust = join . find isJust

day6 :: String -> Int
day6 input = countOrbits (parseInputTree input) 0

--day6b :: String -> Int
day6b input = minimum $ map (\(a,b,c)->b-1+c-1)      
                    $ filter (\(a,b,c)-> b/=0 && c/=0 )
                    $ map (\y -> (name y, stepsToYOU y,stepsToSAN y))
                    $ mapMaybe (`findPlanet` planetTree)
                    (reverse $ takeWhile (\y -> y/="YOU" && y/= "SAN") 
                    $ walkTree planetTree)
    where planetTree = parseInputTree input

--notZero (x,y,z) = case x
name (Planet name planets) =  name
stepsToYOU= countOrbitStepsDirect "YOU"
stepsToSAN = countOrbitStepsDirect "SAN"

data CelestialBody a = CenterOfMass a [CelestialBody a] | Planet a [CelestialBody a]
    deriving (Show, Read, Eq)

findCom :: Map String String -> String
findCom planetMap = findCom' seed planetMap'
    where seed = fst $ Map.elemAt 0 planetMap'
          findCom' key planetMap' = if Map.member key planetMap'
                                   then findCom' (planetMap' Map.! key) planetMap' 
                                   else key
          planetMap'= planetMap -- Map.fromList $ map(\(x,y) -> (y,x) ) $ Map.toList planetMap


input="COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\n"
input2="COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"

parseInput input = map ((\ [a, b] -> (b, a)) . splitOn ")") $ lines input

parseInputTree input = buildTree com planetMap
    where planetMap = foldr (\(a,b) acc -> if Map.member b acc
                                           then Map.insert b (a:(acc Map.! b)) acc
                                           else Map.insert b [a] acc
                        ) Map.empty $ parseInput input
          com = findCom $ Map.fromList $ parseInput input

walkTree :: CelestialBody String-> [String]
walkTree (Planet name planets)=  name : concatMap walkTree planets

buildTree current planetMap = 
        Planet current ( map (`buildTree` planetMap) 
                        $ if Map.member current planetMap
                          then planetMap Map.! current 
                          else [])

countOrbits :: CelestialBody a -> Int -> Int
countOrbits (Planet name planets) count = 
        count + sum (map (\y -> countOrbits y (count+1)) planets )

findAndCountOrbits :: String -> CelestialBody String -> Int 
findAndCountOrbits search (Planet name planets) = findAndCountOrbits' search (Planet name planets) 0 
    where findAndCountOrbits' search (Planet name planets) count = 
                if name==search 
                then count
                else count + sum (map (\y -> 
                            findAndCountOrbits' search y (count+1)) planets )

findPlanet :: String -> CelestialBody String -> Maybe (CelestialBody String)
findPlanet search (Planet name planets)   
    | name==search = Just (Planet name planets)
    | otherwise =  firstJust $ findPlanet search <$> planets
                
countOrbitStepsDirect :: String -> CelestialBody String -> Int
countOrbitStepsDirect search (Planet name planets)= 
                    countOrbitStepsDirect' search (Planet name planets) 0 
    where 
        countOrbitStepsDirect' search (Planet name planets) count = 
            if name==search then count
            else sum $ map (\y -> countOrbitStepsDirect' search y (count+1)) planets 

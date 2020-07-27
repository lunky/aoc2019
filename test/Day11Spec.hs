module Day11Spec where

import Day11
import Test.Hspec
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "move" $ do
        it "should translate a 0 to a counter clockwise movement" $ do
            let curr = Coord(0,0) -- xy coord
            let currOrientation = North
            let direction = 0
            let grid = Map.fromList [] 
            let expected = Move(Coord(-1,0), West, Map.fromList [(Coord(0,0),0)])
            move' direction (Move(curr,currOrientation,grid)) `shouldBe` expected
        it "should translate a 0 to a counter clockwise movement" $ do
            let curr = Coord(1,1) -- xy coord
            let currOrientation = North
            let direction = 0
            let grid = Map.fromList [(Coord(0,0),0)] 
            let expected = Move(Coord(0,1), West, Map.fromList [(Coord(0,0),0),(Coord(1,1),0)])
            move' direction (Move(curr,currOrientation,grid)) `shouldBe` expected
        it "should translate a 0 to a counter clockwise movement" $ do
            let curr = Coord(2,2) -- xy coord
            let currOrientation = East
            let direction = 0
            let grid = Map.fromList [(Coord(0,0),0)] 
            let expected = Move(Coord(2,3), North, Map.fromList [(Coord(0,0),0),(Coord(2,2),0)])
            move' direction (Move(curr,currOrientation,grid)) `shouldBe` expected
        it "should translate a 0 to a counter clockwise movement" $ do
            let curr = Coord(2,2) -- xy coord
            let currOrientation = West
            let direction = 0
            let grid = Map.fromList []
            let expected = Move(Coord(2,1), South, Map.fromList [(Coord(2,2),0)])
            move' direction (Move(curr,currOrientation,grid)) `shouldBe` expected
        it "should translate a 1 to a clockwise movement" $ do
            let curr = Coord(0,0) -- xy coord
            let currOrientation = North
            let direction = 1
            let grid = Map.fromList [] 
            let expected = Move(Coord(1,0), East, Map.fromList [(Coord(0,0),0)])
            move' direction (Move(curr,currOrientation,grid)) `shouldBe` expected
        it "should translate a 1 to a clockwise movement" $ do
            let curr = Coord(1,1) -- xy coord
            let currOrientation = South 
            let direction = 1
            let grid = Map.fromList [] 
            let expected = Move(Coord(0,1), West, Map.fromList [(Coord(1,1),0)])
            move' direction (Move(curr,currOrientation,grid)) `shouldBe` expected
        it "should translate a 1 to a clockwise movement" $ do
            let curr = Coord(-1,-1) -- xy coord
            let currOrientation = West
            let direction = 1
            let grid = Map.fromList [] 
            let expected = Move(Coord(-1,0), North, Map.fromList [(Coord(-1,-1),0)])
            move' direction (Move(curr,currOrientation,grid)) `shouldBe` expected
    --describe "Day11" $ do

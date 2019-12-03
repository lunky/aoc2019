module Day3Spec where

import Day3
import Test.Hspec

spec :: Spec
spec = do
    describe "test harness" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
    describe "move" $ do
      it "should go right" $ do 
        let input = ('R',5)
        let grid = []
        let expected = [(1,0), (2,0), (3,0), (4,0), (5,0)]
        move input (grid,(0,0)) `shouldBe` expected
      it "should go left" $ do 
        let input = ('L',5)
        let grid = []
        let expected = [(-1,0), (-2,0), (-3,0), (-4,0), (-5,0)]
        move input (grid,(0,0)) `shouldBe` expected
      it "should go up" $ do 
        let input = ('U',5)
        let grid = []
        let expected = [(2,3), (2,4), (2,5), (2,6), (2,7)]
        move input (grid,(2,2)) `shouldBe` expected
      it "should go down" $ do 
        let input = ('D',5)
        let grid = []
        let expected = [(2,1), (2,0), (2,-1), (2,-2), (2,-3)]
        move input (grid,(2,2)) `shouldBe` expected
    describe "day3" $ do 
      it "should match sample 1" $ do
        let input =  "R8,U5,L5,D3\nU7,R6,D4,L4"
        let expected = 6
        day3 input `shouldBe` expected
    describe "day3" $ do 
      it "should match pattern 1" $ do
        let input =  "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
        let expected = 159
        day3 input `shouldBe` expected
      it "should match pattern 2" $ do
        let input =  "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
        let expected = 135
        day3 input `shouldBe` expected
    describe "day3b" $ do 
      it "should match sample 1" $ do
        let input =  "R8,U5,L5,D3\nU7,R6,D4,L4"
        let expected = 30
        day3b input `shouldBe` expected
      it "should match sample 1" $ do
        let input =  "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
        let expected = 610
        day3b input `shouldBe` expected
      it "should match sample 3" $ do
        let input =  "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
        let expected = 410
        day3b input `shouldBe` expected

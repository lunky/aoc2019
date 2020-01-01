module Day9Spec where

import Day9
import Test.Hspec

spec :: Spec
spec = 
    describe "Day9" $ do
        it "should run pattern 1" $ do
          let input="109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
          let expected = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
          day9 input [] `shouldBe` expected
        it "should run pattern 2" $ do
          let input="1102,34915192,34915192,7,4,7,99,0"
          let sixteenDigits x = 16 == length (show $ head x)
          day9 input [] `shouldSatisfy` sixteenDigits
        it "should run pattern 3" $ do
          let input="104,1125899906842624,99"
          let expected = [1125899906842624]
          day9 input [] `shouldBe` expected
        it "should run part1 " $ do
          input <- readFile "data/day9.txt"
          let expected = [3546494377]
          day9 input [1] `shouldBe` expected

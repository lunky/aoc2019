module Day7Spec where

import Day7
import Test.Hspec

spec :: Spec
spec = 
    describe "Day7" $ do
      it "noop test" $ do
        1 `shouldBe` 1
      it "calculates the first pattern" $ do
        let input = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
        let expected = 43210
        day7 input `shouldBe` expected
      it "calculates the 2nd pattern" $ do
        let input = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
        let expected = 54321
        day7 input `shouldBe` expected
      it "calculates the 3rd pattern" $ do
        let input = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
        let expected = 65210
        day7 input `shouldBe` expected


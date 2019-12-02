module Day2Spec where

import Day2
import Test.Hspec

spec :: Spec
spec = do
    describe "test harness" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
    describe "Day2" $ do
        it "intCode should match pattern 1" $ do
          let input = [1,9,10,3,2,3,11,0,99,30,40,50]
          let expected = [3500,9,10,70,2,3,11,0,99,30,40,50]
          intCode 0 input `shouldBe` expected
        it "intCode should match pattern 2" $ do
          let input = [1,9,10,70,2,3,11,0,99,30,40,50]
          let expected = [3500,9,10,70,2,3,11,0,99,30,40,50]
          intCode 4 input `shouldBe` expected

        it "intCode should become pattern 1" $ do 
          let input = [1,0,0,0,99] 
          let expected = [2,0,0,0,99] --(1 + 1 = 2).
          intCode 0 input `shouldBe` expected
        it "intCode should become pattern 2" $ do 
          let input = [2,3,0,3,99] 
          let expected = [2,3,0,6,99] --(3 * 2 = 6).
          intCode 0 input `shouldBe` expected
        it "intCode should become pattern 3" $ do 
          let input = [2,4,4,5,99,0] 
          let expected = [2,4,4,5,99,9801] --(99 * 99 = 9801).
          intCode 0 input `shouldBe` expected
        it "intCode should become pattern 4" $ do 
          let input = [1,1,1,4,99,5,6,0,99] 
          let expected = [30,1,1,4,2,5,6,0,99]
          intCode 0 input `shouldBe` expected


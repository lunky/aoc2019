module Day1Spec where

import Day1
import Test.Hspec

spec :: Spec
spec = do
    describe "Lib" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
    describe "day1" $ 
        it "should do sample 1" $ do
            let input = "12"
            let expected = 2 
            day1 input `shouldBe` expected
    describe "day1b" $ do
        it "should do sample 1" $ do
            let input = "14"
            let expected = 2 
            day1b input `shouldBe` expected
        it "should do sample 2" $ do
            let input = "1969"
            let expected = 966
            day1b input `shouldBe` expected
        it "should do sample 3" $ do
            let input = "100756"
            let expected = 50346
            day1b input `shouldBe` expected

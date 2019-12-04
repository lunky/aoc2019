module Day5Spec where

import Day5
import Test.Hspec

spec :: Spec
spec = do
    describe "day5" $ 
        it "should do sample 1" $ do
            let input = ""
            let expected = 0 
            day5 input `shouldBe` expected
    describe "day5b" $ do
        it "should do sample 1" $ do
            let input = ""
            let expected = 0 
            day5b input `shouldBe` expected

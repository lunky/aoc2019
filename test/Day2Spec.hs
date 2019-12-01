module Day2Spec where

import Day2
import Test.Hspec

spec :: Spec
spec = do
    describe "test harness" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1

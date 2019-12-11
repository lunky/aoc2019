module Day7Spec where

import Day7
import Test.Hspec

spec :: Spec
spec = 
    describe "Lib" $ 
        it "should run a noop test" $ 
            1 `shouldBe` 1
    --describe "Day7" $ do

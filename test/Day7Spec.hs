module Day7Spec where

import Day7
import Test.Hspec

spec :: Spec
spec = do
    describe "Lib" $ do
        it "should run a noop test" $ do
            1 `shouldBe` 1
    --describe "Day7" $ do

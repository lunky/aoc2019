module Day6Spec where

import Day6
import Test.Hspec

spec :: Spec
spec = do
    describe "Day6" $ 
        it "should run the sample" $ do
            let input="COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\n"
            let expected=42
            day6 input `shouldBe` expected
--    describe "Day6b" $ 
--        it "should run the sample" $ do
--            let input="COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"
--            let expected=4
--            day6b input `shouldBe` expected

module Day4Spec where

import Day4
import Test.Hspec

spec :: Spec
spec = do
    describe "rule1" $ do
        it "should not find a duplicate adjacent" $ do
          let input = 1234
          let expected = False
          rule1 input `shouldBe` expected
        it "should find a duplicate adjacent" $ do
          let input = 12334
          let expected = True
          rule1 input `shouldBe` expected
    describe "rule2" $ do
        it "should not find an decreasse left to right" $ do
          let input = 1234
          let expected = True
          rule2 input `shouldBe` expected
        it "should find an increase left to right" $ do
          let input = 12342
          let expected = False
          rule2 input `shouldBe` expected
    describe "rule3" $ do
        it "should not find an series of dups > 2" $ do
          let input = 12334
          let expected = True
          rule2 input `shouldBe` expected
        it "should find an series of dups > 2" $ do
          let input = 13334
          let expected = False
          rule3 input `shouldBe` expected

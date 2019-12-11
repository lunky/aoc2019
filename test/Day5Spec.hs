module Day5Spec where

import Day5
import Test.Hspec

{-# ANN module "HLint: ignore Reduce duplication" #-}
spec :: Spec
spec = 
    describe "intCode" $ do
        it "intCode should match pattern 1" $ do
          let input = [1,9,10,3,2,3,11,0,99,30,40,50]
          let expected = ([3500,9,10,70,2,3,11,0,99,30,40,50],0)
          intCode 0 input 0 0 `shouldBe` expected
        it "intCode should match pattern 2" $ do
          let input = [1,9,10,70,2,3,11,0,99,30,40,50]
          let expected = ([3500,9,10,70,2,3,11,0,99,30,40,50],0)
          intCode 4 input 0 0 `shouldBe` expected

        it "intCode should become pattern 1" $ do 
          let input = [1,0,0,0,99] 
          let expected = ([2,0,0,0,99],0) --(1 + 1 = 2).
          intCode 0 input 0 0 `shouldBe` expected
        it "intCode should become pattern 2" $ do 
          let input = [2,3,0,3,99] 
          let expected = ([2,3,0,6,99],0) --(3 * 2 = 6).
          intCode 0 input 0 0 `shouldBe` expected
        it "intCode should become pattern 3" $ do 
          let input = [2,4,4,5,99,0] 
          let expected = ([2,4,4,5,99,9801],0) --(99 * 99 = 9801).
          intCode 0 input 0 0`shouldBe` expected
        it "intCode should become pattern 4" $ do 
          let input = [1,1,1,4,99,5,6,0,99] 
          let expected = ([30,1,1,4,2,5,6,0,99],0)
          intCode 0 input 0 0 `shouldBe` expected
        it "intCode should accept input instruction " $ do 
          let input = [3,3,99,5,6,0,99] 
          let expected = ([3,3,99,88,6,0,99],0)
          intCode 0 input 88 0 `shouldBe` expected
        it "intCode should output value " $ do 
          let input = [4,4,99,5,6,0,99] 
          let expected = ([4,4,99,5,6,0,99],6)
          intCode 0 input 0 0 `shouldBe` expected
        it "should do mode 2" $ do
          let input = [1002,4,3,4,33] 
          let expected = ([1002,4,3,4,99],0)
          intCode 0 input 0 0 `shouldBe` expected
        it "should do negatives" $ do
          let input = [1101,100,-1,4,0]
          let expected = ([1101, 100, -1, 4, 99],0)
          intCode 0 input 0 0 `shouldBe` expected
        it "should do example 1 - true" $ do
          let input = [3,9,8,9,10,9,4,9,99,-1,8]
          let expected = 1
          let (_,response)=intCode 0 input 8 0 
          response `shouldBe` expected
        it "should do example 1 - false " $ do
          let input = [3,9,8,9,10,9,4,9,99,-1,8]
          let input2 = 7 -- not 8
          let expected = 0
          let (_,response)=intCode 0 input input2 0 
          response `shouldBe` expected
        it "should do example 2 - less than" $ do
          let input = [3,9,7,9,10,9,4,9,99,-1,8]
          let input2 = 7 -- less than 8
          let expected = 1
          let (_,response)=intCode 0 input input2 0 
          response `shouldBe` expected
        it "should do example 2 - not less than" $ do
          let input = [3,9,7,9,10,9,4,9,99,-1,8]
          let input2 = 19 -- not less than 8
          let expected = 0
          let (_,response)=intCode 0 input input2 0 
          response `shouldBe` expected
        it "should do example 3 - not equal " $ do
          let input = [3,3,1108,-1,8,3,4,3,99]
          let input2 = 18 -- not equal to 8
          let expected = 0
          let (_,response)=intCode 0 input input2 0 
          response `shouldBe` expected
        it "should do example 3 - equal " $ do
          let input = [3,3,1108,-1,8,3,4,3,99]
          let input2 = 8 -- equal to 8
          let expected = 1
          let (_,response)=intCode 0 input input2 0 
          response `shouldBe` expected
        it "should do example 4 - less than " $ do
          let input = [3,3,1107,-1,8,3,4,3,99]
          let input2 = 3 -- less than 8
          let expected = 1
          let (_,response)=intCode 0 input input2 0 
          response `shouldBe` expected
        it "should do example 5 - jump test" $ do
          let input = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
          let input2 = 0 --input==0 
          let expected = 0
          let (_,response)=intCode 0 input input2 0 
          response `shouldBe` expected
        it "should do example 5 - jump test" $ do
          let input = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
          let input2 = 1 -- input /= 0 
          let expected = 1
          let (_,response)=intCode 0 input input2 0 
          response `shouldBe` expected
        it "should do example 5 - jump test immediate" $ do
          let input = [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
          let input2 = 10 -- input /= 0 
          let expected = 1
          let (_,response)=intCode 0 input input2 0 
          response `shouldBe` expected



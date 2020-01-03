module Day10Spec where

import Day10
import Test.Hspec

spec :: Spec
spec = do
    describe "test harness" $ 
      it "should run a noop test" $ 
          1 `shouldBe` 1
    describe "canSee" $ do
      it "should not see any in pattern 2A" $ do
        let input = [(0,0),(3,1),(6,2),(9,3)]
        let expected = [(3,1)]
        canSee (0,0) input `shouldBe` expected
      it "should not see any in pattern 2B" $ do
        let input = [(0,0),(3,3),(4,4),(5,5),(6,6)]
        let expected = [(3,3)]
        canSee (0,0) input `shouldBe` expected
      it "should not see any in pattern 2C" $ do
        let input = [(0,0),(3,2),(6,4),(9,6)]
        let expected = [(3,2)]
        canSee (0,0) input `shouldBe` expected
      it "should not see any in pattern 2D" $ do
        let input = [(0,0),(2,3),(4,6),(6,9)]
        let expected = [(2,3)]
        canSee (0,0) input `shouldBe` expected
      it "should not see any in pattern 2E" $ do
        let input = [(0,0),(1,3),(2,6),(3,9)]
        let expected = [(1,3)]
        canSee (0,0) input `shouldBe` expected
      it "should not see any in pattern 2F" $ do
        let input = [(0,0),(2,4),(3,6),(4,8)]
        let expected = [(2,4)]
        canSee (0,0) input `shouldBe` expected
      it "should not see any in pattern 2G" $ do
        let input = [(0,0),(4,3),(8,6)]
        let expected = [(4,3)]
        canSee (0,0) input `shouldBe` expected
      it "should not see only A-G" $ do
         let input= parseInput "#.........\n...#......\n...#..#...\n.####....#\n..#.#.#...\n.....#....\n..###.#.##\n.......#..\n....#...#.\n...#..#..#\n"
         let expected = [(3,1),(4,3),(2,4),(2,3),(3,2),(3,3),(1,3)]
         canSee (0,0) input `shouldMatchList` expected

    describe "Day10" $ do
      it "should get answer for pattern 1" $ do
        let input = ".#..#\n.....\n#####\n....#\n...##\n"
        let expected = ((3,4),8)
        day10 input `shouldBe` expected
      it "should get answer for pattern 3" $ do
        let input = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"
        let expected = ((5,8),33)
        day10 input `shouldBe` expected
      it "should get answer for pattern 4" $ do
        let input = "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###."
        let expected = ((1,2),35)
        day10 input `shouldBe` expected
      it "should get answer for pattern 5" $ do
        let input = ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..\n"
        let expected = ((6,3),41)
        day10 input `shouldBe` expected

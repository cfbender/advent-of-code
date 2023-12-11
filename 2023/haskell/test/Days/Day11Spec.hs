module Days.Day11Spec where

import Days.Day11
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 11" $ do
    it "returns the correct answer for the test input for part A" $ do
      let testText = "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#....."
      let Just testInput = parseTest inputParser testText
      partA testInput `shouldBe` 374

    it "returns the correct answer for the test input for part B" $ do
      let testText = "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#....."
      let Just testInput = parseTest inputParser testText
      partB testInput `shouldBe` 82000210

module Days.Day09Spec where

import Days.Day09
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 09" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "2333133121414131402"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 1928

    it "returns the correct answer for the test input for part 2" $ do
      let testText = "2333133121414131402"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` 2858

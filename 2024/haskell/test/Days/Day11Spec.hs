module Days.Day11Spec where

import Days.Day11
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 11" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "125 17"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 55312

    it "returns the correct answer for the test input for part 2" $ do
      let testText = "125 17"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` 65601038650482

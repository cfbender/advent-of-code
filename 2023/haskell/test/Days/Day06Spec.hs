module Days.Day06Spec where

import Days.Day06
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 06" $ do
    it "returns the correct answer for the test input for part A" $ do
      let testText = "Time:      7  15   30\nDistance:  9  40  200"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 288

    it "returns the correct answer for the test input for part B" $ do
      let testText = "Time:      7  15   30\nDistance:  9  40  200"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` 71503

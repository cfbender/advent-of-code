module Days.Day09Spec where

import Days.Day09
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 09" $ do
    it "returns the correct answer for the test input for part A" $ do
      let testText = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 114

    it "returns the correct answer for the test input for part B" $ do
      let testText = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` 2

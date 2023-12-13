module Days.Day07Spec where

import Days.Day07
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 07" $ do
    it "returns the correct answer for the test input for part A" $ do
      let testText = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 6440

    it "returns the correct answer for the test input for part B" $ do
      let testText = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` 5905

module Days.Day01Spec where

import Days.Day01
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 01" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` (11 :: Int)

    it "returns the correct answer for the test input for part 2" $ do
      let testText = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` (31 :: Int)

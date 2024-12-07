module Days.Day07Spec where

import Days.Day07
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 07" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 3749

    it "returns the correct answer for the test input for part 2" $ do
      let testText = "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` 11387

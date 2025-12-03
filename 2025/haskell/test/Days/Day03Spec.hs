module Days.Day03Spec where

import Days.Day03
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 03" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "987654321111111\n811111111111119\n234234234234278\n818181911112111"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 357

    it "returns the correct answer for the test input for part 2" $ do
      let testText = "987654321111111\n811111111111119\n234234234234278\n818181911112111"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` 3121910778619

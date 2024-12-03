module Days.Day02Spec where

import Days.Day02
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 02" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` (2 :: Int)

    it "returns the correct answer for the test input for part 2" $ do
      let testText = "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` (4 :: Int)

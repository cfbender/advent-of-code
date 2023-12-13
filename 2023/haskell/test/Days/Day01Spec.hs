module Days.Day01Spec where

import Days.Day01
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 01" $ do
    it "returns the correct answer for the test input for part A" $ do
      let Just testInput = parseTest inputParser "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
      part1 testInput `shouldBe` (142 :: Int)

    it "returns the correct answer for the test input for part B" $ do
      let testText = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` (281 :: Int)

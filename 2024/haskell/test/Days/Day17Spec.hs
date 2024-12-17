module Days.Day17Spec where

import Days.Day17
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 17" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "Register A: 729\nRegister B: 0\nRegister C: 0\n\nProgram: 0,1,5,4,3,0"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` "4,6,3,5,6,3,5,2,1,0"

    it "returns the correct answer for the test input for part 2" $ do
      let testText = "Register A: 2024\nRegister B: 0\nRegister C: 0\n\nProgram: 0,3,5,4,3,0"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` 117440

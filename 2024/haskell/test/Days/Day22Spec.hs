module Days.Day22Spec where

import Days.Day22
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 22" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "1\n10\n100\n2024"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 37327623

    it "returns the correct answer for the test input for part 2" $ do
      let testText = ""
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` undefined

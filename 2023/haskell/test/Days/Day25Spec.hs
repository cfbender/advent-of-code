module Days.Day25Spec where

import Days.Day25
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 25" $ do
    it "returns the correct answer for the test input for part A" $ do
      let testText = ""
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` undefined

    it "returns the correct answer for the test input for part B" $ do
      let testText = ""
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` undefined

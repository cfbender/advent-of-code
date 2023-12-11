module Days.Day13Spec where

import Days.Day13
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 13" $ do
    it "returns the correct answer for the test input for part A" $ do
      let testText = ""
      let Just testInput = parseTest inputParser testText
      partA testInput `shouldBe` undefined

    it "returns the correct answer for the test input for part B" $ do
      let testText = ""
      let Just testInput = parseTest inputParser testText
      partB testInput `shouldBe` undefined

module Days.Day19Spec where

import Days.Day19
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 19" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "r, wr, b, g, bwu, rb, gb, br\n\nbrwrr\nbggr\ngbbr\nrrbgbr\nubwu\nbwurrg\nbrgr\nbbrgwb"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 6

    it "returns the correct answer for the test input for part 2" $ do
      let testText = ""
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` undefined

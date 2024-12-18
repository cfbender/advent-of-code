module Days.Day18Spec where

import Days.Day18
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 18" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "5,4\n4,2\n4,5\n3,0\n2,1\n6,3\n2,4\n1,5\n0,6\n3,3\n2,6\n5,1\n1,2\n5,5\n2,5\n6,5\n1,4\n0,4\n6,4\n1,1\n6,1\n1,0\n0,5\n1,6\n2,0"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 146

    it "returns the correct answer for the test input for part 2" $ do
      let testText = ""
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` (-1, -1)

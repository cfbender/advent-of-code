module Days.Day12Spec where

import Days.Day12
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 12" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 1930

    it "returns the correct answer for the test input for part 2" $ do
      let testText = "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE"
      let testText = "AAAAAA\nAAABBA\nAAABBA\nABBAAA\nABBAAA\nAAAAAA"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` 368

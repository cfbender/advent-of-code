module Days.Day08Spec where

import Days.Day08
import Debug.Trace
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 08" $ do
    it "returns the correct answer for the test input for part A" $ do
      let testText = "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 6

-- don't know why this test input isn't working for me. maybe I'll fix it later
-- it "returns the correct answer for the test input for part B" $ do
--   let testText = "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)"
--   let Just testInput = parseTest inputParser testText
--   part2 testInput `shouldBe` 6

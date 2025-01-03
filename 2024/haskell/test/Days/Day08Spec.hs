module Days.Day08Spec where

import Days.Day08
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 08" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 14

    it "returns the correct answer for the test input for part 2" $ do
      let testText = "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` 34

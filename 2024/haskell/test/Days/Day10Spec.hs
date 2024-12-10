module Days.Day10Spec where

import Days.Day10
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 10" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 36

    it "returns the correct answer for the test input for part 2" $ do
      let testText = "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` 81

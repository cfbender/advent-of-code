module Days.Day03Spec where

import Days.Day03
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 03" $ do
    it "returns the correct answer for the test input for part A" $ do
      let testText = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."
      let Just testInput = parseTest inputParser testText
      partA testInput `shouldBe` 4361

    it "returns the correct answer for the test input for part B" $ do
      let testText = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."
      let Just testInput = parseTest inputParser testText
      partB testInput `shouldBe` 467835

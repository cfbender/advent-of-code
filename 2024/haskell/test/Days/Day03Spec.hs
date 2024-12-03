module Days.Day03Spec where

import Days.Day03
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 03" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` (161 :: Int)

    it "returns the correct answer for the test input for part 2" $ do
      let testText = ""
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` undefined

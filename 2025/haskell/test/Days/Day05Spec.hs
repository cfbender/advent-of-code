module Days.Day05Spec where

import Days.Day05
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
    describe "Day 05" $ do
        it "returns the correct answer for the test input for part 1" $ do
            let testText = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"
            let Just testInput = parseTest inputParser testText
            part1 testInput `shouldBe` 3

        it "returns the correct answer for the test input for part 2" $ do
            let testText = "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"
            let Just testInput = parseTest inputParser testText
            part2 testInput `shouldBe` 14

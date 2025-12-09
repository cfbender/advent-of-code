module Days.Day06Spec where

import Days.Day06
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
    describe "Day 06" $ do
        it "returns the correct answer for the test input for part 1" $ do
            -- put in a row of numbers to match the count of the real input
            let testText = "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n  1 0     1 0  \n*   +   *   +  "
            let Just testInput = parseTest inputParser testText
            part1 testInput `shouldBe` 4277556

        it "returns the correct answer for the test input for part 2" $ do
            -- put in a row of numbers to match the count of the real input
            let testText = "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n  1 0     1 0  \n*   +   *   +  "
            let Just testInput = parseTest inputParser testText
            let Just testInput = parseTest inputParser testText
            -- answer different due to extra digits
            part2 testInput `shouldBe` 32650667

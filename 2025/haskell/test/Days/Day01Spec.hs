module Days.Day01Spec where

import Days.Day01
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
    describe "Day 01" $ do
        it "returns the correct answer for the test input for part 1" $ do
            let testText = "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82"
            let Just testInput = parseTest inputParser testText
            part1 testInput `shouldBe` 3

        it "returns the correct answer for the test input for part 2" $ do
            let testText = "R1000\nL1050\nL68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82"
            let Just testInput = parseTest inputParser testText
            part2 testInput `shouldBe` 25

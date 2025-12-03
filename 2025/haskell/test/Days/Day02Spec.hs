module Days.Day02Spec where

import Days.Day02
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
    describe "Day 02" $ do
        it "returns the correct answer for the test input for part 1" $ do
            let testText = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
            let Just testInput = parseTest inputParser testText
            part1 testInput `shouldBe` 1227775554

        it "returns the correct answer for the test input for part 2" $ do
            let testText = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
            let Just testInput = parseTest inputParser testText
            part2 testInput `shouldBe` 4174379265

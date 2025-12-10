module Days.Day08Spec where

import Days.Day08
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
    describe "Day 08" $ do
        it "returns the correct answer for the test input for part 1" $ do
            let testText = "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689"
            let Just testInput = parseTest inputParser testText
            solve 10 testInput `shouldBe` 40

        it "returns the correct answer for the test input for part 2" $ do
            let testText = "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689"
            let Just testInput = parseTest inputParser testText
            part2 testInput `shouldBe` 25272

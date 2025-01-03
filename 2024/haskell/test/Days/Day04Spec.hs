module Days.Day04Spec where

import Days.Day04
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 04" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` (18 :: Int)

    it "returns the correct answer for the test input for part 2" $ do
      let testText = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` (9 :: Int)

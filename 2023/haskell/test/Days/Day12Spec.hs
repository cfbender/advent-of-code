module Days.Day12Spec where

import Days.Day12
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 12" $ do
    it "returns the correct answer for the test input for part A" $ do
      let testText = "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 21

    it "returns the correct answer for the test input for part B" $ do
      let testText = "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` 525152

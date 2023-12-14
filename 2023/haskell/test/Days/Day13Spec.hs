module Days.Day13Spec where

import Days.Day13
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 13" $ do
    it "returns the correct answer for the test input for part A" $ do
      let testText = "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 405

    it "returns the correct answer for the test input for part B" $ do
      let testText = "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#"
      let Just testInput = parseTest inputParser testText
      part2 testInput `shouldBe` 400

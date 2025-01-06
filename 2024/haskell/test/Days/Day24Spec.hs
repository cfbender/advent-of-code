module Days.Day24Spec where

import Days.Day24
import Test.Hspec
import Test.QuickCheck
import Util.Helpers (parseTest)

spec :: Spec
spec = do
  describe "Day 24" $ do
    it "returns the correct answer for the test input for part 1" $ do
      let testText = "x00: 1\nx01: 0\nx02: 1\nx03: 1\nx04: 0\ny00: 1\ny01: 1\ny02: 1\ny03: 1\ny04: 1\n\nntg XOR fgs -> mjb\ny02 OR x01 -> tnw\nkwq OR kpj -> z05\nx00 OR x03 -> fst\ntgd XOR rvg -> z01\nvdt OR tnw -> bfw\nbfw AND frj -> z10\nffh OR nrd -> bqk\ny00 AND y03 -> djm\ny03 OR y00 -> psh\nbqk OR frj -> z08\ntnw OR fst -> frj\ngnj AND tgd -> z11\nbfw XOR mjb -> z00\nx03 OR x00 -> vdt\ngnj AND wpb -> z02\nx04 AND y00 -> kjc\ndjm OR pbm -> qhw\nnrd AND vdt -> hwm\nkjc AND fst -> rvg\ny04 OR y02 -> fgs\ny01 AND x02 -> pbm\nntg OR kjc -> kwq\npsh XOR fgs -> tgd\nqhw XOR tgd -> z09\npbm OR djm -> kpj\nx03 XOR y03 -> ffh\nx00 XOR y04 -> ntg\nbfw OR bqk -> z06\nnrd XOR fgs -> wpb\nfrj XOR qhw -> z04\nbqk OR frj -> z07\ny03 OR x01 -> nrd\nhwm AND bqk -> z03\ntgd XOR rvg -> z12\ntnw OR pbm -> gnj"
      let Just testInput = parseTest inputParser testText
      part1 testInput `shouldBe` 2024

    it "returns the correct answer for the test input for part 2" $ do
      let testText = "x00: 0\nx01: 1\nx02: 0\nx03: 1\nx04: 0\nx05: 1\ny00: 0\ny01: 0\ny02: 1\ny03: 1\ny04: 0\ny05: 1\n\nx00 AND y00 -> z05\nx01 AND y01 -> z02\nx02 AND y02 -> z01\nx03 AND y03 -> z03\nx04 AND y04 -> z04\nx05 AND y05 -> z00"
      let Just testInput = parseTest inputParser testText
      -- part2 testInput `shouldBe` "z00,z01,z02,z05"
      part2 testInput `shouldBe` undefined

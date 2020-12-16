defmodule AocTest do
  use ExUnit.Case
  doctest Aoc

  test "ges all valid values" do
    assert Aoc.gen_rule_range(["1-3", "5-7"]) == MapSet.new([1, 2, 3, 5, 6, 7])
  end

  test "processes rules" do
    assert Aoc.process_rules([["departure location", "1-3 or 5-7"]]) ==
             [{:rule, "departure location", MapSet.new([1, 2, 3, 5, 6, 7])}]
  end

  test "parts" do
    assert Aoc.main() == {32835, 514_662_805_187}
  end
end

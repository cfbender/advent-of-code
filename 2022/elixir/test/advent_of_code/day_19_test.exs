defmodule AdventOfCode.Day19Test do
  use ExUnit.Case

  import AdventOfCode.Day19

  @tag timeout: :infinity
  test "part1" do
    input =
      """
      Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
      Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
      """
      |> parse_input()

    result = part1(input)

    assert result == 33
  end

  @tag :skip
  test "part2" do
    input =
      """
      """
      |> parse_input()

    result = part2(input)

    assert result
  end
end

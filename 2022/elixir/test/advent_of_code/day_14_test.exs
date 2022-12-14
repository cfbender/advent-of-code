defmodule AdventOfCode.Day14Test do
  use ExUnit.Case

  import AdventOfCode.Day14

  test "part1" do
    input =
      """
      498,4 -> 498,6 -> 496,6
      503,4 -> 502,4 -> 502,9 -> 494,9
      """
      |> parse_input()

    result = part1(input)

    assert result == 24
  end

  test "part2" do
    input =
      """
      498,4 -> 498,6 -> 496,6
      503,4 -> 502,4 -> 502,9 -> 494,9
      """
      |> parse_input()

    result = part2(input)

    assert result == 93
  end
end

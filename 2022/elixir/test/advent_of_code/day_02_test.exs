defmodule AdventOfCode.Day02Test do
  use ExUnit.Case

  import AdventOfCode.Day02

  test "part1" do
    input =
      """
      A Y
      B X
      C Z
      """
      |> parse_input()

    result = part1(input)

    assert result == 15
  end

  test "part2" do
    input =
      """
      A Y
      B X
      C Z
      """
      |> parse_input()

    result = part2(input)

    assert result == 12
  end
end

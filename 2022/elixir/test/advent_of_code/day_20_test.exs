defmodule AdventOfCode.Day20Test do
  use ExUnit.Case

  import AdventOfCode.Day20

  test "part1" do
    input =
      """
      1
      2
      -3
      3
      -2
      0
      4
      """
      |> parse_input()

    result = part1(input)

    assert result == 3
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

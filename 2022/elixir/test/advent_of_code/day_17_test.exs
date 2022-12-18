defmodule AdventOfCode.Day17Test do
  use ExUnit.Case

  import AdventOfCode.Day17

  test "part1" do
    input =
      """
      >>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
      """
      |> parse_input()

    result = part1(input)

    assert result == 3068
  end

  @tag timeout: :infinity
  @tag :skip
  test "part2" do
    input =
      """
      >>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
      """
      |> parse_input()

    result = part2(input)

    assert result == 1_514_285_714_288
  end
end

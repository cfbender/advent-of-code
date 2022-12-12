defmodule AdventOfCode.Day12Test do
  use ExUnit.Case

  import AdventOfCode.Day12

  test "part1" do
    input =
      """
      Sabqponm
      abcryxxl
      accszExk
      acctuvwj
      abdefghi
      """
      |> parse_input()

    result = part1(input)

    assert result == 31
  end

  test "part2" do
    input =
      """
      Sabqponm
      abcryxxl
      accszExk
      acctuvwj
      abdefghi
      """
      |> parse_input()

    result = part2(input)

    assert result == 29
  end
end

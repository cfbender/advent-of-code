defmodule AdventOfCode.Day23Test do
  use ExUnit.Case

  import AdventOfCode.Day23

  test "part1" do
    input =
      """
      ....#..
      ..###.#
      #...#.#
      .#...##
      #.###..
      ##.#.##
      .#..#..
      """
      |> parse_input()

    result = part1(input)

    assert result == 110
  end

  test "part2" do
    input =
      """
      ....#..
      ..###.#
      #...#.#
      .#...##
      #.###..
      ##.#.##
      .#..#..
      """
      |> parse_input()

    result = part2(input)

    assert result == 20
  end
end

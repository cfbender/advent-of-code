defmodule AdventOfCode.Day22Test do
  use ExUnit.Case

  import AdventOfCode.Day22

  test "part1" do
    input =
      "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"
      |> parse_input()

    result = part1(input, true)

    assert result == 6032
  end

  test "part2" do
    input =
      "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"
      |> parse_input()

    result = part2(input, true)

    assert result == 5031
  end
end

defmodule AdventOfCode.Day23Test do
  use ExUnit.Case

  import AdventOfCode.Day23

  test "part1" do
    input = [
      "#############",
      "#...........#",
      "###B#C#B#D###",
      "#A#D#C#A#",
      "#########"
    ]

    result = part1(input)

    assert result == 12521
  end

  @tag timeout: :infinity
  test "part2" do
    input = [
      "#############",
      "#...........#",
      "###B#C#B#D###",
      "#A#D#C#A#",
      "#########"
    ]

    result = part2(input)

    assert result == 44169
  end
end

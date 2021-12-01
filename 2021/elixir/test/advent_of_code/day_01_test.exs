defmodule AdventOfCode.Day01Test do
  use ExUnit.Case

  import AdventOfCode.Day01

  test "part1" do
    input = [
      199,
      200,
      208,
      210,
      200,
      207,
      240,
      269,
      260,
      263
    ]

    result = part1(input)

    assert result == 7
  end

  test "part2" do
    input = [
      199,
      200,
      208,
      210,
      200,
      207,
      240,
      269,
      260,
      263
    ]

    result = part2(input)

    assert result == 5
  end
end

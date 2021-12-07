defmodule AdventOfCode.Day07Test do
  use ExUnit.Case

  import AdventOfCode.Day07

  test "part1" do
    input = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]
    result = part1(input)

    assert result == 37
  end

  test "part2" do
    input = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]
    result = part2(input)

    assert result == 168
  end
end

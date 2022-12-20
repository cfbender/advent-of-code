defmodule AdventOfCode.Day20Test do
  use ExUnit.Case

  import AdventOfCode.Day20

  @test_input """
  1
  2
  -3
  3
  -2
  0
  4
  """
  test "part1" do
    input =
      @test_input
      |> parse_input()

    result = part1(input)

    assert result == 3
  end

  test "part2" do
    input =
      @test_input
      |> parse_input()

    result = part2(input)

    assert result == 1_623_178_306
  end
end

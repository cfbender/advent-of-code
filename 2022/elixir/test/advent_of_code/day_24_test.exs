defmodule AdventOfCode.Day24Test do
  use ExUnit.Case

  import AdventOfCode.Day24

  test "part1" do
    input =
      """
      #.######
      #>>.<^<#
      #.<..<<#
      #>v.><>#
      #<^v^^>#
      ######.#
      """
      |> parse_input()

    result = part1(input, true)

    assert result == 18
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

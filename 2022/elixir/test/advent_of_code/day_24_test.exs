defmodule AdventOfCode.Day24Test do
  use ExUnit.Case

  import AdventOfCode.Day24

  # @tag :skip
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

  test "part2" do
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

    result = part2(input, true)

    assert result == 54
  end
end

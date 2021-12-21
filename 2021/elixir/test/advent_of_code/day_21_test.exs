defmodule AdventOfCode.Day21Test do
  use ExUnit.Case

  import AdventOfCode.Day21

  test "part1" do
    input = [4, 8]
    result = part1(input)

    assert result == 739_785
  end

  @tag timeout: :infinity
  test "part2" do
    input = [4, 8]
    result = part2(input)

    assert result == 444_356_092_776_315
  end
end

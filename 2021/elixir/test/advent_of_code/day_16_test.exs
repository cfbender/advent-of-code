defmodule AdventOfCode.Day16Test do
  use ExUnit.Case

  import AdventOfCode.Day16

  test "part1" do
    inputs = [
      "8A004A801A8002F478",
      "620080001611562C8802118E34",
      "C0015000016115A2E0802F182340",
      "A0016C880162017C3686B18A3D4780"
    ]

    results = Enum.map(inputs, &part1(&1))

    assert results == [16, 12, 23, 31]
  end

  @tag :skip
  test "part2" do
    input = nil
    result = part2(input)

    assert result
  end
end

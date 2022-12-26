defmodule AdventOfCode.Day25Test do
  use ExUnit.Case

  import AdventOfCode.Day25

  test "parses SNAFU numbers" do
    input =
      """
      1=-0-2
      12111
      2=0=
      21
      2=01
      111
      20012
      112
      1=-1=
      1-12
      12
      1=
      122
      1
      2
      1=
      1-
      10
      11
      12
      2=
      2-
      20
      1=0
      1-0
      1=11-2
      1-0---0
      1121-1110-1=0
      """
      |> parse_input()

    results = [
      1747,
      906,
      198,
      11,
      201,
      31,
      1257,
      32,
      353,
      107,
      7,
      3,
      37,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      15,
      20,
      2022,
      12345,
      314_159_265
    ]

    parsed = Enum.map(input, &parse_snafu/1)

    assert parsed == results
  end

  test "converts to snafu numbers" do
    input =
      """
      1=-0-2
      12111
      2=0=
      21
      2=01
      111
      20012
      112
      1=-1=
      1-12
      12
      1=
      122
      1
      2
      1=
      1-
      10
      11
      12
      2=
      2-
      20
      1=0
      1-0
      1=11-2
      1-0---0
      1121-1110-1=0
      """
      |> parse_input()

    results = [
      1747,
      906,
      198,
      11,
      201,
      31,
      1257,
      32,
      353,
      107,
      7,
      3,
      37,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      15,
      20,
      2022,
      12345,
      314_159_265
    ]

    parsed = Enum.map(results, &decimal_to_snafu/1)

    assert parsed == input
  end

  test "part1" do
    input =
      """
      1=-0-2
      12111
      2=0=
      21
      2=01
      111
      20012
      112
      1=-1=
      1-12
      12
      1=
      122
      """
      |> parse_input()

    result = part1(input)

    assert result == "2=-1=0"
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

defmodule AdventOfCode.Day25 do
  @moduledoc """
  Done! 2022 in the books. Definitely the worst of the 3 years I've done so far,
  too many pathfinding algorithms, too many problems not well-explained or example
  data that doesn't catch enough cases. Probably the hardest time I've had, and the 
  problems felt like a net-negative to my life in the second half. But got through it!

  Maybe I'm just getting dumber.
  """
  import AdventOfCode.Helpers

  def parse_input(input) do
    input
    |> lines()
  end

  def parse_snafu(snafu) do
    String.codepoints(snafu)
    |> Enum.reverse()
    |> Enum.with_index()
    |> Enum.reduce(0, fn {part, i}, sum ->
      case Integer.parse(part) do
        {x, ""} ->
          x * Integer.pow(5, i) + sum

        :error ->
          case part do
            "=" -> sum - Integer.pow(5, i) * 2
            "-" -> sum - Integer.pow(5, i)
          end
      end
    end)
  end

  def decimal_to_snafu(dec) do
    digits = %{0 => 0, 1 => 1, 2 => 2, 3 => "=", 4 => "-"}
    # check up to 100 digits in base 5
    Enum.reduce_while(0..100, {[], dec}, fn _x, {acc, r} ->
      # snafu digits start at -2, so add two to bring back to 0
      quotient = div(r + 2, 5)

      # find the value in the map that corresponds to the remainder 
      new_list = [digits[rem(r, 5)] | acc]

      if quotient == 0 do
        {:halt, new_list}
      else
        {:cont, {new_list, quotient}}
      end
    end)
    |> Enum.join()
  end

  def part1(input) do
    Enum.map(input, &parse_snafu/1)
    |> Enum.sum()
    |> decimal_to_snafu()
  end

  def part2(_args) do
  end
end

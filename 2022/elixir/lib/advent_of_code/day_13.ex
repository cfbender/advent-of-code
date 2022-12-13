defmodule AdventOfCode.Day13 do
  @moduledoc """
  """
  def parse_input(input) do
    input
    |> String.split("\n\n", trim: true)
    |> Enum.map(fn group ->
      String.split(group, "\n", trim: true)
      |> Enum.map(fn line ->
        {result, _} = Code.eval_string(line)
        result
      end)
    end)
  end

  # def ordered?(left, right) when is_integer(left) and is_integer(right), do: left < right

  # If both values are integers, the lower integer should come first.
  def ordered?([lh | _lt], [rh | _rt]) when is_integer(lh) and is_integer(rh) and lh != rh,
    do: lh < rh

  # If both values are equal, continue.
  def ordered?([lh | lt], [rh | rt]) when is_integer(lh) and is_integer(rh), do: ordered?(lt, rt)

  # If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.
  def ordered?([lh | lt], [rh | rt]),
    do: if(ordered?(lh, rh), do: ordered?(lt, rt), else: false)

  # Lists were same length and no decision was made.
  def ordered?([], []), do: true
  # If the right list runs out of items first, the inputs are not in the right order.
  def ordered?([_ | _], []), do: false
  # If the left list runs out of items first, the inputs are in the right order.
  def ordered?([], [_ | _]), do: true

  # If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison.
  def ordered?(left, right) when is_list(left) and is_integer(right), do: ordered?(left, [right])
  def ordered?(left, right) when is_list(right) and is_integer(left), do: ordered?([left], right)

  def part1(input) do
    input
    |> Enum.with_index()
    |> Enum.map(fn {pair, i} -> {pair, i + 1} end)
    |> Enum.filter(fn {[left, right], i} ->
      IO.inspect(i)
      ordered?(left, right)
    end)
    |> Enum.map(&elem(&1, 1))
    |> Enum.sum()
  end

  def part2(_args) do
  end
end

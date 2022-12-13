defmodule AdventOfCode.Day13 do
  @moduledoc """
  This one really stumped me. Unfortunately I got at least 9 unique answers that passed the test input,
  but not the full input. Those are always my least favorite. I think it's a case of both unclear instructions (ie. comparing two different
  integers where the left was < right should halt the search), and not enough edge cases in the example input.

  Part 2 was easy enough once I got the bubble sort working.
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

  # If both values are equal, continue.
  def ordered?(left, right) when is_integer(left) and is_integer(right) and left == right,
    do: :no_decision

  # If both values are integers, the lower integer should come first.
  def ordered?(left, right) when is_integer(left) and is_integer(right), do: left < right

  def ordered?([lh | lt], [rh | rt]) do
    is_ordered = ordered?(lh, rh)

    # If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.
    if is_ordered == :no_decision do
      ordered?(lt, rt)
    else
      is_ordered
    end
  end

  # Lists were same length and no decision was made.
  def ordered?([], []), do: :no_decision
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
    |> Enum.filter(fn {[left, right], _i} ->
      ordered?(left, right)
    end)
    |> Enum.map(&elem(&1, 1))
    |> Enum.sum()
  end

  def sort_order(left, right) do
    order = ordered?(left, right)

    case order do
      :no_decision -> true
      _ -> order
    end
  end

  def part2(input) do
    two_packet = [[2]]
    six_packet = [[6]]

    input =
      [
        two_packet,
        six_packet
      ] ++
        Enum.reduce(input, [], fn [left, right], acc -> [left, right | acc] end)

    sorted = Enum.sort(input, &sort_order/2)

    two_idx = Enum.find_index(sorted, fn item -> item == two_packet end) + 1
    six_idx = Enum.find_index(sorted, fn item -> item == six_packet end) + 1

    two_idx * six_idx
  end
end

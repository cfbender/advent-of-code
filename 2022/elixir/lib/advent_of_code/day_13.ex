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

  def longest_length(left, right) when length(left) > length(right), do: length(left)
  def longest_length(_left, right), do: length(right)

  # If both values are integers, the lower integer should come first.
  def ordered?(left, right) when is_integer(left) and is_integer(right) and left != right,
    do: {:halt, left < right}

  # Otherwise, the inputs are the same integer; continue checking the next part of the input.
  def ordered?(left, right) when is_integer(left) and is_integer(right),
    do: {:cont, true}

  # Two empty lists are equivalent
  def ordered?([], []), do: {:cont, true}

  # If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.
  def ordered?(nil, nil), do: {:cont, true}
  # If the right list runs out of items first, the inputs are not in the right order.
  def ordered?(_left, nil), do: {:halt, false}
  # If the left list runs out of items first, the inputs are in the right order.
  def ordered?(nil, _right), do: {:halt, true}

  # If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison.
  def ordered?(left, right) when is_list(left) and is_integer(right), do: ordered?(left, [right])
  def ordered?(left, right) when is_list(right) and is_integer(left), do: ordered?([left], right)

  # If both values are lists, compare the first value of each list, then the second value, and so on.
  def ordered?(left, right, return \\ {:cont, true}) when is_list(left) and is_list(right) do
    case return do
      {:cont, true} ->
        Enum.reduce_while(0..(longest_length(left, right) - 1), true, fn idx, _acc ->
          left = Enum.at(left, idx)
          right = Enum.at(right, idx)
          return = ordered?(left, right)

          case return do
            {:halt, val} -> {:halt, {:halt, val}}
            _ -> {:cont, {:cont, true}}
          end
        end)

      {:halt, val} ->
        {:halt, val}
    end
  end

  def part1(input) do
    input
    |> Enum.with_index()
    |> Enum.map(fn {pair, i} -> {pair, i + 1} end)
    |> Enum.filter(fn {[left, right], _i} ->
      {:halt, val} = ordered?(left, right)
      val
    end)
    |> Enum.map(&elem(&1, 1))
    |> Enum.sum()
  end

  def sort(list) when is_list(list) do
    pass = sort_pass(list)
    if pass == list, do: pass, else: sort(pass)
  end

  def sort_pass([x, y | t]) do
    {:halt, ordered} = ordered?(x, y)

    if ordered do
      [x | sort_pass([y | t])]
    else
      [y | sort_pass([x | t])]
    end
  end

  def sort_pass(list), do: list

  def part2(input) do
    two_packet = [[2]]
    six_packet = [[6]]

    input =
      [
        two_packet,
        six_packet
      ] ++
        Enum.reduce(input, [], fn [left, right], acc -> [left, right | acc] end)

    sorted = sort(input)
    two_idx = Enum.find_index(sorted, fn item -> item == two_packet end) + 1
    six_idx = Enum.find_index(sorted, fn item -> item == six_packet end) + 1

    two_idx * six_idx
  end
end

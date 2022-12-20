defmodule AdventOfCode.Day20 do
  @moduledoc """
  Getting tired of problems where the puzzle is finding the edge cases the problem doesn't tell you about.
  I am surprised he didn't give a "and here's a larger example" for this one, which would've helped immensely.

  Thanks to @JasonNoonan for helping me figure out a nice way to effectively unique the numbers by pairing them with
  their original index, I got this one very quickly after that. The cycling was very un-intuitive, so the code gets a bit fiddly.

  Also elixir's lists aren't super performant here since they are linked, but it's only a 5000 length list so who cares.
  """
  import AdventOfCode.Helpers

  def parse_input(input) do
    input
    |> lines()
    |> Enum.map(&String.to_integer/1)
  end

  @decryption_key 811_589_153
  def move(list, length, {num, _}, idx) do
    new_idx = rem(idx + num, length - 1)

    cond do
      # subtract an extra one because lists aren't zero indexed from the end
      new_idx < 0 -> new_idx - 1
      new_idx > 0 -> new_idx
      new_idx == 0 -> nil
    end
    |> case do
      nil ->
        Enum.slide(list, idx, length)

      wrapped_idx ->
        Enum.slide(list, idx, wrapped_idx)
    end
  end

  def mix(list, mixed, length) do
    Enum.reduce(list, mixed, fn num, acc ->
      idx = Enum.find_index(acc, &(&1 == num))
      move(acc, length, num, idx)
    end)
  end

  def get_answer(result, length) do
    zero = Enum.find_index(result, fn {x, _} -> x == 0 end)

    Enum.map([1000, 2000, 3000], fn x ->
      Enum.at(result, rem(zero + x, length))
      |> elem(0)
    end)
    |> Enum.sum()
  end

  def part1(input) do
    length = length(input)

    Enum.with_index(input)
    |> then(fn input -> mix(input, input, length) end)
    |> get_answer(length)
  end

  def part2(input) do
    length = length(input)

    input =
      Enum.map(input, &(&1 * @decryption_key))
      |> Enum.with_index()

    Enum.reduce(0..9, input, fn _, acc ->
      mix(input, acc, length)
    end)
    |> get_answer(length)
  end
end

defmodule AdventOfCode.Day14 do
  def parse_rules(raw_rules) do
    Stream.map(raw_rules, fn rule ->
      [pair, insertion] = String.split(rule, " -> ", trim: true)

      {String.codepoints(pair), insertion}
    end)
    |> Enum.into(%{})
  end

  def polymerize(polymer, pairs, steps, count \\ 0)
  def polymerize(polymer, _pairs, steps, count) when steps == count, do: polymer

  def polymerize(polymer, pairs, steps, count) do
    Enum.reduce(pairs, polymer, fn {[a, b] = pair, insertion}, acc ->
      if Map.has_key?(polymer, pair) do
        # inserting changes count of 3 pairs
        # eg. AC -> B creates AB, BC, removes an AC
        # this is done as many times as there were that matching pair
        Enum.chunk_every([a, insertion, b], 2, 1, :discard)
        |> Enum.reduce(acc, fn new_pair, pair_acc ->
          Map.update(pair_acc, new_pair, polymer[pair], &(&1 + polymer[pair]))
        end)
        |> Map.update!(pair, &(&1 - polymer[pair]))
      else
        acc
      end
    end)
    |> Enum.filter(fn {_k, v} -> v > 0 end)
    |> Enum.into(%{})
    |> polymerize(pairs, steps, count + 1)
  end

  def count_monomers(polymer, beginning, ending) do
    # beginning and end monomers won't overlap twice so start with one each
    Enum.reduce(polymer, %{beginning => 1, ending => 1}, fn {[a, b], count}, acc ->
      Map.update(acc, a, count, &(&1 + count))
      |> Map.update(b, count, &(&1 + count))
    end)
    |> Enum.map(fn {monomer, count} -> {monomer, floor(count / 2)} end)
    |> Enum.into(%{})
  end

  def part1(input, count \\ 10) do
    [[template], rules] = input

    split_template = String.codepoints(template)

    [beginning | _rest] = split_template
    ending = List.last(split_template)

    {min, max} =
      split_template
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.frequencies()
      |> polymerize(parse_rules(rules), count)
      |> count_monomers(beginning, ending)
      |> Map.values()
      |> Enum.min_max()

    max - min
  end

  def part2(input) do
    part1(input, 40)
  end
end

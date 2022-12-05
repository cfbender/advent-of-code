defmodule AdventOfCode.Day05 do
  @moduledoc """
  Now we are rampin up baby! 95% of my time here was parsing the input. The rest worked out really nicely
  with a good data structure. Really fun problem!

  I think this could do with some optimization on the nested reduces, probably a cleaner way to do most of this.
  """
  def parse_input(input) do
    [crates, instructions] =
      input
      |> String.split("\n\n", trim: true)

    instructions =
      String.split(instructions, "\n", trim: true)
      |> Enum.map(fn line ->
        [_, count, source, target] = Regex.run(~r/move (\d+) from (\d+) to (\d+)/, line)
        [count, source, target] |> Enum.map(&String.to_integer/1) |> List.to_tuple()
      end)

    {
      String.split(crates, "\n", trim: true)
      |> Stream.drop(-1)
      |> Stream.map(&String.graphemes/1)
      |> Stream.map(&Enum.chunk_every(&1, 4))
      |> Enum.reduce(%{}, fn level, acc ->
        Enum.with_index(level)
        |> Enum.reduce(acc, fn {curr, idx}, inner_acc ->
          if Enum.any?(curr, &(&1 != " ")) do
            crate = Enum.find(curr, &(&1 not in ["[", " ", "]"]))
            Map.update(inner_acc, idx + 1, [crate], &[crate | &1])
          else
            inner_acc
          end
        end)
      end)
      # crates are reversed to make removal/addition easier in linked lists
      |> Stream.map(fn {k, v} -> {k, Enum.reverse(v)} end)
      |> Enum.into(%{}),
      instructions
    }
  end

  def part1(input) do
    {stacks, instructions} = input

    Enum.reduce(instructions, stacks, fn {count, source, target}, acc ->
      # do instruction n times
      Enum.reduce(1..count, acc, fn _curr, inner_acc ->
        [to_move | left] = inner_acc[source]

        Map.put(inner_acc, source, left)
        |> Map.update!(target, &[to_move | &1])
      end)
    end)
    |> Enum.map(fn {_k, [top | _]} -> top end)
    |> Enum.join()
  end

  def part2(input) do
    {stacks, instructions} = input

    Enum.reduce(instructions, stacks, fn {count, source, target}, acc ->
      {to_move, left} = Enum.split(acc[source], count)

      Map.put(acc, source, left)
      |> Map.update!(target, &(to_move ++ &1))
    end)
    |> dbg()
    |> Enum.map(fn {_k, [top | _]} -> top end)
    |> Enum.join()
  end
end

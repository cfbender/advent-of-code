defmodule AdventOfCode.Day06 do
  @moduledoc """
  Got hung up on the data structure I parsed it into with the chunks and indexes.
  Could've done it quicker, but now I definitely remember how to chunk with overlap!
  """
  def parse_input(input) do
    input
    |> String.graphemes()
  end

  def find_uniq_seq(input, window) do
    input
    |> Enum.with_index()
    |> Enum.chunk_every(window, 1, :discard)
    |> Enum.find(fn chunk ->
      chunk = Enum.map(chunk, &elem(&1, 0))
      Enum.uniq(chunk) == chunk
    end)
  end

  def part1(input) do
    {_chunk, idx} =
      input
      |> find_uniq_seq(4)
      |> List.last()

    idx + 1
  end

  def part2(input) do
    {_chunk, idx} =
      input
      |> find_uniq_seq(14)
      |> List.last()

    idx + 1
  end
end

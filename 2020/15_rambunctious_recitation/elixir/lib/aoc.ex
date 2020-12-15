require Record

defmodule Aoc do
  Record.defrecord(:history, first: 1, last: 1)

  def get_input() do
    {:ok, input} = File.read("../input.txt")
    input |> String.split(",", trim: true)
  end

  def process_nums(1, _mem, 2020, prev), do: prev
  def process_nums(2, _mem, 30_000_000, prev), do: prev

  def process_nums(part, mem, count, prev) do
    {:history, first, last} =
      mem
      |> Map.get(prev, history(first: count, last: count))

    # first == last, first time num spoken
    spoken = if first == last, do: 0, else: last - first

    # get spoken values previous last, to set as new first \\ default to count + 1
    {:history, _first, prev_last} =
      mem |> Map.get(spoken, history(first: count + 1, last: count + 1))

    # put new value for spoken in mem
    new_mem =
      mem
      |> Map.put(spoken, history(first: prev_last, last: count + 1))

    process_nums(part, new_mem, count + 1, spoken)
  end

  def main do
    input = get_input()

    int_nums = input |> Enum.map(&String.to_integer/1)

    mem =
      int_nums
      |> Enum.with_index()
      |> Enum.map(fn {x, idx} ->
        {x, history(first: idx + 1, last: idx + 1)}
      end)
      |> Map.new()

    [1, 2]
    |> Enum.map(fn x ->
      Task.async(fn -> process_nums(x, mem, Enum.count(int_nums), List.last(int_nums)) end)
    end)
    |> Task.await_many(60000)
    |> List.to_tuple()
  end
end

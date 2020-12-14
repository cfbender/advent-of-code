defmodule AoC do
  def get_input do
    {:ok, input} = File.read("../input.txt")

    [timestamp, ids] = input |> String.split("\n", trim: true)

    {timestamp |> String.to_integer(),
     ids
     |> String.split(",", trim: true)
     |> Enum.map(fn x -> if x == "x", do: x, else: String.to_integer(x) end)}
  end

  def part1(ids, time) do
    result =
      ids
      |> Enum.find(fn
        x when x != "x" ->
          rem(time, x) == 0

        _ ->
          false
      end)

    if result, do: {time, result}, else: part1(ids, time + 1)
  end

  def part2(ids, curr_idx, time, step) do
    if curr_idx == Enum.count(ids) do
      time
    else
      {id, offset} = ids |> Enum.at(curr_idx)

      cond do
        rem(time + offset, id) == 0 ->
          part2(ids, curr_idx + 1, time, step * id)

        true ->
          part2(ids, curr_idx, time + step, step)
      end
    end
  end

  def main() do
    {timestamp, ids} = get_input()
    {time, part_1_id} = part1(ids, timestamp)
    part_1_answer = (time - timestamp) * part_1_id

    part_2_time =
      ids |> Enum.with_index() |> Enum.filter(fn {x, _idx} -> x !== "x" end) |> part2(0, 0, 1)

    {part_1_answer, part_2_time}
  end
end

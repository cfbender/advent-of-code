defmodule Aoc do
  def get_input do
    [9, 6, 2, 7, 1, 3, 8, 5, 4]
  end

  def create_map(list) do
    [first | _] = list
    cycle = list ++ [first]

    cycle |> Enum.chunk_every(2, 1, :discard) |> Map.new(fn [k, v] -> {k, v} end)
  end

  def get_next_cup(curr, pickup, limits) do
    {min, max} = limits

    cond do
      curr < min ->
        get_next_cup(max, pickup, limits)

      curr not in pickup ->
        curr

      true ->
        get_next_cup(curr - 1, pickup, limits)
    end
  end

  def play(cups, _limits, _sel, 100, 1), do: cups
  def play(cups, _limits, _sel, 10_000_000, 2), do: cups

  def play(cups, limits, sel, round, part) do
    # follow list 4 deep to get pickup and next selected
    [first_pick, middle_pick, last_pick, next_sel] =
      0..3
      |> Enum.reduce([], fn _x, acc ->
        cond do
          Enum.empty?(acc) ->
            [Map.get(cups, sel)]

          true ->
            acc ++ [Map.get(cups, acc |> List.last())]
        end
      end)

    dest = get_next_cup(sel - 1, [first_pick, middle_pick, last_pick], limits)

    # set current to point to next selected, destination to point to the first of the pickup, 
    # and the last of the pickup to point to where the destination used to point
    %{cups | sel => next_sel, dest => first_pick, last_pick => cups[dest]}
    |> play(limits, next_sel, round + 1, part)
  end

  def main do
    input = get_input()

    part1_res =
      input |> create_map() |> play(input |> Enum.min_max(), input |> List.first(), 0, 1)

    part2_input =
      input ++
        (1..1_000_000
         |> Enum.reduce([], fn x, acc -> if x in input, do: acc, else: [x | acc] end)
         |> Enum.reverse())

    part2_result =
      part2_input
      |> create_map()
      |> play(part2_input |> Enum.min_max(), part2_input |> List.first(), 0, 2)

    {1..(length(input) - 1)
     # start at one and get the remaining
     |> Enum.reduce([], fn x, acc ->
       case x do
         1 ->
           [Map.get(part1_res, x)]

         _ ->
           acc ++ [Map.get(part1_res, acc |> List.last())]
       end
     end)
     |> Enum.join(""), part2_result[1] * part2_result[part2_result[1]]}
  end
end

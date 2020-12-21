defmodule Aoc do
  def get_input do
    input = File.read!("../input.txt")
    allergen_regex = ~r"(.*)\s\(contains\s(.*)\)"

    input
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      [_, ingredients, allergens] = allergen_regex |> Regex.run(line)
      [ingredients |> String.split(" ", trim: true), allergens |> String.split(", ", trim: true)]
    end)
  end

  def translate([], _list, mem, _count), do: mem

  def translate(allergens, list, mem, count) do
    allergen = Stream.cycle(allergens) |> Enum.at(count)

    used =
      mem
      |> Map.values()
      |> Enum.reduce(MapSet.new(), fn x, acc -> acc |> MapSet.union(x) end)

    [first | rest] =
      list
      |> Enum.filter(fn [_, allergens] ->
        allergens |> Enum.member?(allergen)
      end)
      |> Enum.map(&List.first/1)

    new_vals =
      rest
      |> Enum.reduce(
        first |> MapSet.new(),
        fn item, acc ->
          item |> MapSet.new() |> MapSet.intersection(acc)
        end
      )
      |> MapSet.difference(used)

    case new_vals |> MapSet.size() do
      1 ->
        next_mem = mem |> Map.put(allergen, new_vals)

        next_allergens = allergens |> List.delete(allergen)
        translate(next_allergens, list, next_mem, count + 1)

      _ ->
        # pass to next
        translate(allergens, list, mem, count + 1)
    end
  end

  def find_all(list) do
    all_allergens = list |> Enum.map(fn [_, al] -> al end) |> List.flatten() |> MapSet.new()

    translated =
      all_allergens
      |> MapSet.to_list()
      |> translate(list, Map.new(), 0)

    safe =
      list
      |> Enum.map(&List.first/1)
      |> List.flatten()
      |> MapSet.new()
      |> MapSet.difference(
        translated
        |> Map.values()
        |> Enum.reduce(MapSet.new(), fn x, acc -> acc |> MapSet.union(x) end)
      )

    {translated, safe}
  end

  def main do
    input = get_input()
    {translated, safe} = input |> find_all()

    part1 =
      input
      |> Enum.reduce(0, fn [ingreds, _], acc ->
        acc + (ingreds |> Enum.count(&(safe |> MapSet.member?(&1))))
      end)

    part2 =
      translated
      |> Map.to_list()
      |> List.keysort(0)
      |> Enum.map(fn {_, set} -> set |> MapSet.to_list() |> List.first() end)
      |> Enum.join(",")

    {part1, part2}
  end
end

require Record

defmodule Aoc do
  Record.defrecord(:rule, field: "", valid: MapSet.new())

  def gen_rule_range(rule) do
    rule
    |> Enum.map(fn x ->
      String.split(x, "-", trim: true) |> Enum.map(fn y -> y |> String.to_integer() end)
    end)
    |> Enum.reduce(MapSet.new(), fn [min, max], acc ->
      Enum.to_list(min..max)
      |> Enum.reduce(acc, fn x, new_set ->
        new_set |> MapSet.put(x)
      end)
    end)
  end

  def process_rules(rules) do
    rule_regex = ~r/^(\d+-\d+) or (\d+-\d+)$/

    rules
    |> Enum.map(fn [field, rule] ->
      valid =
        Regex.run(rule_regex, rule)
        |> List.delete_at(0)
        |> gen_rule_range()

      rule(field: field, valid: valid)
    end)
  end

  def get_input do
    {:ok, input} = File.read("../input.txt")
    [rules, ticket, nearby] = input |> String.split("\n\n") |> Enum.map(&String.split(&1, "\n"))

    rules =
      rules
      |> Enum.map(&String.split(&1, ": ", trim: true))
      |> process_rules()

    ticket = ticket |> Enum.at(1) |> String.split(",") |> Enum.map(&String.to_integer/1)

    nearby =
      nearby
      |> List.delete_at(0)
      |> Enum.map(fn x -> x |> String.split(",", trim: true) |> Enum.map(&String.to_integer/1) end)

    {rules, ticket, nearby}
  end

  def part1([], rate, _rules), do: rate

  def part1(nearby, rate, rules) do
    [curr | rest] = nearby

    sum_errs =
      curr
      |> Enum.reduce(0, fn x, acc -> acc + if rules |> MapSet.member?(x), do: 0, else: x end)

    part1(rest, rate + sum_errs, rules)
  end

  def part2(_cols, [], _check, mem), do: mem

  def part2(cols, rules, check, mem) do
    check_stream = Stream.cycle(cols |> Map.keys())
    col_idx = check_stream |> Enum.at(check)
    col_nums = cols |> Map.get(col_idx)

    matches =
      rules
      |> Enum.filter(fn {:rule, _field, valid} ->
        col_nums |> Enum.all?(fn x -> valid |> MapSet.member?(x) end)
      end)

    cond do
      matches |> Enum.count() === 1 ->
        match = matches |> List.first()
        {:rule, field, _vals} = match
        new_mem = mem |> Map.put(field, col_idx)
        part2(cols, rules |> List.delete(match), check, new_mem)

      true ->
        part2(cols, rules, check + 1, mem)
    end
  end

  def main do
    {rules, ticket, nearby} = get_input()

    all_valid =
      rules
      |> Enum.reduce(MapSet.new(), fn {:rule, _field, set}, acc -> acc |> MapSet.union(set) end)

    part1 =
      part1(
        nearby,
        0,
        all_valid
      )

    valid_tickets =
      nearby
      |> Enum.filter(fn tic -> tic |> Enum.all?(fn x -> all_valid |> MapSet.member?(x) end) end)

    cols =
      valid_tickets
      |> Enum.flat_map(&Enum.with_index/1)
      |> Enum.group_by(fn {_x, idx} -> idx end, fn {x, _idx} -> x end)

    dep_rules =
      rules
      |> Enum.filter(fn {:rule, field, _val} -> field |> String.starts_with?("departure") end)

    part2 =
      part2(cols, dep_rules, 0, Map.new())
      |> Enum.reduce(1, fn {_field, idx}, acc ->
        acc * (ticket |> Enum.at(idx))
      end)

    {part1, part2}
  end
end

defmodule Aoc do
  def get_input do
    {:ok, input} = File.read("../input.txt")

    [rules, messages] =
      input |> String.split("\n\n", trim: true) |> Enum.map(&String.split(&1, "\n", trim: true))

    map_rules =
      rules
      |> Enum.reduce(Map.new(), fn x, acc ->
        [num, rule] = x |> String.split(":")

        acc
        |> Map.put(
          num,
          rule |> String.replace("\"", "") |> String.split("|") |> Enum.map(&String.trim/1)
        )
      end)

    {map_rules, messages}
  end

  def to_regex(str_list) do
    ["(?:" <> Enum.join(str_list, "|") <> ")"]
  end

  def process_rules(rule, _mem) when rule in [["a"], ["b"]], do: rule

  def process_rules(rule, mem) do
    rule
    |> Enum.map(fn x ->
      x
      |> String.split(" ", trim: true)
      |> Enum.map(fn
        # comment out guards for part 1
        num when num === "8" ->
          "#{mem |> Map.get("42") |> process_rules(mem) |> List.to_string()}+"

        num when num === "11" ->
          rule_42 = mem |> Map.get("42") |> process_rules(mem) |> List.to_string()
          rule_31 = mem |> Map.get("31") |> process_rules(mem) |> List.to_string()

          # check up to 5 deep (any deeper throws a regex too large error)
          1..5
          |> Enum.map(fn x ->
            "#{rule_42}{#{x}}#{rule_31}{#{x}}"
          end)
          |> IO.inspect()
          |> to_regex

        num ->
          mem |> Map.get(num) |> process_rules(mem) |> List.to_string()
      end)
    end)
    |> to_regex()
  end

  def main do
    {rules, messages} = get_input()

    {:ok, matcher} =
      "^#{process_rules(rules |> Map.get("0"), rules) |> List.first()}$"
      |> Regex.compile()

    messages |> Enum.count(fn rule -> matcher |> Regex.match?(rule) end)
  end
end

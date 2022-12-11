defmodule AdventOfCode.Day11 do
  @moduledoc """
  Really don't like the ones where you just have to know the math trick.
  Had to look up solutions to learn the LCM trick. Just mod by the product of all 
  of the divisors to cap the numbers at the remainder of their LCM.

  Kinda lame, maybe I should just know more math. Got it really quickly after learning the trick.
  """
  import AdventOfCode.Helpers

  def get_operation(string) do
    [type, x] = String.split(string)

    op =
      case type do
        "*" -> &Kernel.*/2
        "+" -> &Kernel.+/2
      end

    case x do
      "old" ->
        fn old -> old * old end

      _ ->
        x = String.to_integer(x)
        fn old -> op.(x, old) end
    end
  end

  def get_test([condition, if_true, if_false]) do
    divisor = Regex.run(~r/\d+/, condition) |> Enum.at(0) |> String.to_integer()
    true_monkey = Regex.run(~r/\d+/, if_true) |> Enum.at(0) |> String.to_integer()
    false_monkey = Regex.run(~r/\d+/, if_false) |> Enum.at(0) |> String.to_integer()

    {divisor,
     fn x, divisor ->
       if rem(x, divisor) == 0,
         do: {true, true_monkey},
         else: {false, false_monkey}
     end}
  end

  def parse_input(input) do
    input
    |> String.split("\n\n", trim: true)
    |> Enum.map(fn monkey ->
      [monkey, items, operation | test] = String.split(monkey, "\n", trim: true)
      monkey = Regex.run(~r/Monkey (\d):/, monkey) |> Enum.at(1) |> String.to_integer()
      items = Regex.scan(~r/\d+/, items)
      [_, operation] = Regex.run(~r/Operation: new = old (.*)/, operation)
      {divisor, test} = get_test(test)

      {monkey,
       %{
         items: Enum.map(items, fn [num] -> String.to_integer(num) end) |> :queue.from_list(),
         operation: get_operation(operation),
         test: test,
         divisor: divisor,
         inspected: 0
       }}
    end)
    |> Enum.into(%{})
  end

  def mod(monkeys), do: Enum.reduce(monkeys, 1, fn {_, monkey}, acc -> acc * monkey.divisor end)

  def pass(state, items) do
    Enum.reduce(items, state, fn {item, {_, target}}, a ->
      Map.update!(a, target, fn monkey ->
        Map.update!(monkey, :items, fn items -> enqueue(items, item) end)
      end)
    end)
  end

  def run(input, count, opts \\ []) do
    relieved = opts[:relieved]

    monkey_nums = Map.keys(input)
    lcm = mod(input)

    Enum.reduce(1..count, input, fn _, monkeys ->
      Enum.reduce(monkey_nums, monkeys, fn num, acc ->
        %{items: items, operation: operation, test: test, divisor: divisor} = acc[num]

        items =
          :queue.to_list(items)
          |> Enum.map(fn worry ->
            operation.(worry)
            |> then(fn x -> if relieved, do: x, else: Integer.floor_div(x, 3) end)
            |> rem(lcm)
          end)

        to_move = Enum.map(items, fn x -> {x, test.(x, divisor)} end)

        pass(acc, to_move)
        |> Map.update!(num, fn monkey ->
          Map.put(monkey, :items, :queue.new())
          |> Map.update!(:inspected, fn x -> x + length(items) end)
        end)
      end)
    end)
  end

  def part1(input) do
    input
    |> run(20)
    |> Enum.map(fn {_, monkey} -> monkey.inspected end)
    |> Enum.sort(:desc)
    |> Enum.take(2)
    |> Enum.product()
  end

  def part2(input) do
    input
    |> run(10000, relieved: true)
    |> Enum.map(fn {_, monkey} -> monkey.inspected end)
    |> Enum.sort(:desc)
    |> Enum.take(2)
    |> Enum.product()
  end
end

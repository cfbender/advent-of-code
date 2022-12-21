defmodule AdventOfCode.Day21 do
  @moduledoc """
  This was a tough one but I really liked it. Part 1 was mega-easy, but thinking how to do recursive algebra
  was a bit of a brainfuck. The operator commutation tripped me up for a bit, but I eventually got there.

  Another case though where the test input (I assume intentionally) leaves out cases like a known value being
  on the left side of a non-commutative operator. I get that it's part of the puzzle, but I wish he would include those
  so that the test input should pass for any larger input. I hate digging to find out what I did wrong.
  """
  import AdventOfCode.Helpers

  def parse_input(input) do
    input
    |> lines()
    |> Enum.map(fn line ->
      [monkey, yell] = String.split(line, ":", trim: true) |> Enum.map(&String.trim/1)

      yell =
        if String.length(yell) <= 5 do
          String.to_integer(yell)
        else
          [m1, op, m2] = String.split(yell)

          op = String.to_atom(op)
          {m1, op, m2}
        end

      {monkey, yell}
    end)
    |> Enum.into(%{})
  end

  def get_monkey(input, monkey), do: {monkey, input[monkey]}

  def is_human?({_m, yell}), do: yell == nil

  def perform_op(a, op, b) do
    case op do
      :+ -> a + b
      :- -> a - b
      :/ -> div(a, b)
      :* -> a * b
    end
  end

  def get_yell(_input, {_m, y}) when is_integer(y), do: y

  def get_yell(input, {_m, {m1, op, m2}}) do
    monkey_a = get_monkey(input, m1)
    monkey_b = get_monkey(input, m2)

    # can't find monkey (human), return nil up the chain
    if is_human?(monkey_a) or is_human?(monkey_b) do
      nil
    else
      a = get_yell(input, monkey_a)
      b = get_yell(input, monkey_b)

      # one of the dependent values are human, return nil up the chain
      if is_nil(a) or is_nil(b) do
        nil
      else
        perform_op(a, op, b)
      end
    end
  end

  @doc """
  Finds the fully resolved values of both sides of some monkey's equation,
  then return tuple of unknown monkey, the value of the known,
  and the side the known value was on (so it can be used for non-commutative operators later)
  """
  def known_value(input, key) do
    {m1, _, m2} = input[key]
    monkey_a = get_monkey(input, m1)
    monkey_b = get_monkey(input, m2)

    # this could probably do with some refactoring
    cond do
      # need to make these checks first because passing {"humn", nil} to get_yell will be mad
      is_human?(monkey_a) ->
        {monkey_a, get_yell(input, monkey_b), :right}

      is_human?(monkey_b) ->
        {monkey_b, get_yell(input, monkey_a), :left}

      true ->
        a_yell = get_yell(input, monkey_a)
        b_yell = get_yell(input, monkey_b)

        if is_nil(a_yell) do
          {monkey_a, b_yell, :right}
        else
          {monkey_b, a_yell, :left}
        end
    end
  end

  # example worked out
  # 150 known from root side that returns a value
  # 150 = cczh / 4
  # 600 = 4 + lgvd
  # 596 = 2 * ptdq
  # 298 = humn - 3
  # 301 = humn

  def invert_op(op) do
    case op do
      :+ -> :-
      :- -> :+
      :/ -> :*
      :* -> :/
    end
  end

  def find_yell(_input, {m, _yell}, known) when m == "humn", do: known

  @doc """
     The idea here is to find the unknown value in the current monkey's yell
     to flip the equation to that side.
     ie. 150 = cczh / 4 --> 150 * 4 = cczh. Now we can recurse with the carry value of 600
     and go until we are finding the human value, at which point we return the carry
  """
  def find_yell(input, {m, {_m1, op, _m2}}, known) do
    inverted = invert_op(op)
    {unknown, known_start, side} = known_value(input, m)

    # flip operations on left side for non-commutative operators
    new_known =
      if side == :left and op in [:/, :-] do
        perform_op(known_start, op, known)
      else
        perform_op(known, inverted, known_start)
      end

    find_yell(input, unknown, new_known)
  end

  def part1(input) do
    root = {"root", input["root"]}

    input
    |> get_yell(root)
  end

  def part2(input) do
    input = Map.delete(input, "humn")
    {unknown, known_start, _side} = known_value(input, "root")

    find_yell(input, unknown, known_start)
  end
end

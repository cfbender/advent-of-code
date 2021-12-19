defmodule AdventOfCode.Day18 do
  def eval_list(input) do
    all_vals = 0..9 |> Enum.map(&Integer.to_string/1) |> Enum.concat(["[", ",", "]"])
    safe = String.codepoints(input) |> Enum.all?(&(&1 in all_vals))

    if safe do
      {list, _} = Code.eval_string(input)
      list
    else
      raise "you stinker"
    end
  end

  def split(num) when is_integer(num) and num >= 10,
    do: {:split, [Integer.floor_div(num, 2), ceil(num / 2)]}

  def split(num) when is_integer(num), do: num

  def split([left, right]) do
    # mark tuples with atom to know which side splits
    with {:l, l} when not is_tuple(l) <- {:l, split(left)},
         {:r, r} when not is_tuple(r) <- {:r, split(right)} do
      [l, r]
    else
      # left side split, return split left with original right
      {:l, {:split, l}} -> {:split, [l, right]}
      # right side split, return split right with original left
      {:r, {:split, r}} -> {:split, [left, r]}
    end
  end

  # pass along number if called with nil
  def add_to_neighbor(num, nil, _type), do: {num, nil}
  # return nil since number has been used up
  def add_to_neighbor(num, amount, _type) when is_integer(num), do: {num + amount, nil}

  def add_to_neighbor([l, r], amount, :l) do
    {new_l, add_l} = add_to_neighbor(l, amount, :l)
    {[new_l, r], add_l}
  end

  def add_to_neighbor([l, r], amount, :r) do
    {new_r, add_r} = add_to_neighbor(r, amount, :r)
    {[l, new_r], add_r}
  end

  def explode(num, _) when is_integer(num), do: num
  # return 0 (base value after explode) and what to add to left and right
  def explode([left, right], depth) when is_integer(left) and is_integer(right) and depth >= 4,
    do: {:exploded, 0, left, right}

  def explode([left, right], depth) do
    with {:l, l} when not is_tuple(l) <- {:l, explode(left, depth + 1)},
         {:r, r} when not is_tuple(r) <- {:r, explode(right, depth + 1)} do
      [l, r]
    else
      # keeping add terms to keep matching simpler
      # left side exploded, return exploded left with original right, adding exploded right
      {:l, {:exploded, l, add_l, add_r}} ->
        {r, add_r} = add_to_neighbor(right, add_r, :l)
        {:exploded, [l, r], add_l, add_r}

      # right side exploded, return exploded right with original left, adding exploded left
      {:r, {:exploded, r, add_l, add_r}} ->
        {l, add_l} = add_to_neighbor(left, add_l, :r)
        {:exploded, [l, r], add_l, add_r}
    end
  end

  def add(l, r), do: reduce([l, r])

  def reduce(term) do
    with term when is_list(term) <- explode(term, 0),
         term when is_list(term) <- split(term) do
      term
    else
      {:split, term} -> reduce(term)
      {:exploded, term, _, _} -> reduce(term)
    end
  end

  def magnitude([l, r]), do: 3 * magnitude(l) + 2 * magnitude(r)
  def magnitude(n), do: n

  def part1(input) do
    input
    |> Enum.map(&eval_list/1)
    |> Enum.reduce(&add(&2, &1))
    |> magnitude()
  end

  def part2(input) do
    nums = input |> Enum.map(&eval_list/1)

    for(x <- nums, y <- nums, x != y, do: {x, y})
    |> Stream.map(fn {a, b} ->
      [add(a, b), add(b, a)] |> Enum.map(&magnitude/1) |> Enum.max()
    end)
    |> Enum.max()
  end
end

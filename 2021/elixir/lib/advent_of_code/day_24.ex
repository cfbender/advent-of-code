defmodule AdventOfCode.Day24 do
  def parse_function_parameters(program) do
    # get each section of program
    Enum.chunk_every(program, 18)
    |> Enum.map(fn section ->
      # get 3 unique values as parameters
      for(
        x <- [4, 5, 15],
        do: Enum.at(section, x) |> String.split(" ") |> List.last() |> String.to_integer()
      )
      |> List.to_tuple()
    end)
  end

  # https://www.reddit.com/r/adventofcode/comments/rnejv5/2021_day_24_solutions/hpsv5hl/?context=3
  def monad({a, b, _c}, z, w) when rem(z, 26) + b == w, do: Integer.floor_div(z, a)
  def monad({a, _b, c}, z, w), do: Integer.floor_div(z, a) * 26 + w + c

  def monad_min_max(params_list, zs \\ %{0 => {0, 0}})
  def monad_min_max([], zs), do: zs

  def monad_min_max([params = {a, _, _} | rest_params], zs) do
    new_zs =
      Enum.reduce(zs, Map.new(), fn {z, {min, max}}, z_acc ->
        Enum.reduce(1..9, z_acc, fn digit, new_z_acc ->
          # map all possible next digits to new z value
          new_z = monad(params, z, digit)

          if a == 1 or (a == 26 and new_z < z) do
            {new_min, new_max} = {min * 10 + digit, max * 10 + digit}

            Map.update(new_z_acc, new_z, {new_min, new_max}, fn {curr_min, curr_max} ->
              {min(new_min, curr_min), max(new_max, curr_max)}
            end)
          else
            # skip function call that won't help
            new_z_acc
          end
        end)
      end)

    monad_min_max(rest_params, new_zs)
  end

  def part1(input) do
    parse_function_parameters(input)
    |> monad_min_max()
    |> Map.get(0)
    |> elem(1)
  end

  def part2(input) do
    parse_function_parameters(input)
    |> monad_min_max()
    |> Map.get(0)
    |> elem(0)
  end
end

defmodule AdventOfCode.Day11 do
  def get_map(input) do
    input
    |> Enum.map(fn line ->
      String.codepoints(line) |> Enum.map(&String.to_integer/1)
    end)
    |> Enum.with_index()
    |> Enum.reduce(Map.new(), fn {row, y}, acc ->
      Enum.with_index(row)
      |> Enum.reduce(acc, fn {val, x}, row_acc ->
        Map.put(row_acc, {x, y}, {val, false})
      end)
    end)
  end

  def get_adj(map, {x, y}) do
    for(x <- [-1, 0, 1], y <- [-1, 0, 1], do: {x, y})
    |> Enum.reduce(Map.new(), fn {dx, dy}, acc ->
      new_point = {x + dx, y + dy}
      new_point_val = Map.get(map, new_point)

      if new_point_val do
        Map.put(acc, new_point, new_point_val)
      else
        acc
      end
    end)
  end

  def flash(grid, point) do
    get_adj(grid, point)
    |> Enum.reduce(grid, fn {point, _val}, acc ->
      Map.update(acc, point, 1, fn {val, flashed} -> {val + 1, flashed} end)
    end)
    # set current to flashed (doesn't need to be set to 0, just easier than getting the value)
    |> Map.put(point, {0, true})
  end

  def handle_flashes(grid) do
    if Enum.all?(grid, fn {_point, {val, flashed}} -> val <= 9 or flashed end) do
      final_grid =
        Enum.reduce(grid, grid, fn {point, {_val, flashed}}, acc ->
          if flashed, do: Map.put(acc, point, {0, flashed}), else: acc
        end)

      num_flashed = Enum.count(final_grid, fn {_point, {_val, flashed}} -> flashed end)

      # return fully flashed grid and count of flashed
      {final_grid, num_flashed}
    else
      # get all non-flashed above 9 energy and flash them, then repeat
      Enum.filter(grid, fn {_point, {val, flashed}} -> val > 9 and not flashed end)
      |> Enum.reduce(grid, fn {point, _val}, acc -> flash(acc, point) end)
      |> handle_flashes()
    end
  end

  def step(grid) do
    # add 1 to all and reset flashed status
    Enum.reduce(grid, grid, fn {point, {val, _flashed}}, acc ->
      Map.put(acc, point, {val + 1, false})
    end)
  end

  def run_steps(grid, finish, step \\ 0, count \\ 0)
  def run_steps(_grid, finish, step, count) when step == finish, do: count

  def run_steps(grid, finish, step, count) do
    {new_grid, new_count} =
      step(grid)
      |> handle_flashes()

    run_steps(new_grid, finish, step + 1, count + new_count)
  end

  def find_sync(grid, count \\ 0) do
    {new_grid, _new_count} =
      step(grid)
      |> handle_flashes()

    if Enum.all?(new_grid, fn {_point, {_val, flashed}} -> flashed end) do
      count + 1
    else
      find_sync(new_grid, count + 1)
    end
  end

  def part1(input) do
    input
    |> get_map()
    |> run_steps(100)
  end

  def part2(input) do
    input
    |> get_map()
    |> find_sync()
  end
end

defmodule Aoc do
  def get_input do
    input = File.read!("../input.txt")
    tile_regex = ~r"Tile (\d+):"

    input
    |> String.split("\n\n", trim: true)
    |> Enum.reduce(Map.new(), fn tile, acc ->
      [tile_num | grid] = tile |> String.split("\n", trim: true)
      num = tile_regex |> Regex.run(tile_num) |> Enum.at(1) |> String.to_integer()
      acc |> Map.put(num, grid |> Enum.join("\n"))
    end)
  end

  def flip(grid) do
    grid |> String.split("\n") |> Enum.reverse() |> Enum.join("\n")
  end

  def rotate(grid) do
    grid
    |> String.split("\n", trim: true)
    |> Enum.map(fn row -> row |> String.split("", trim: true) end)
    |> Enum.flat_map(&Enum.with_index/1)
    # take all column indices (position in row) and group together
    |> Enum.group_by(fn {_x, idx} -> idx end, fn {x, _idx} -> x end)
    |> Enum.map(fn {_idx, col} -> col |> Enum.reverse() |> Enum.join() end)
    |> Enum.join("\n")
  end

  def get_edges(tile) do
    lines = tile |> String.split("\n")

    left =
      lines
      |> Enum.reduce([], fn line, acc -> [line |> String.first() | acc] end)
      |> Enum.reverse()
      |> Enum.join()

    right =
      lines
      |> Enum.reduce([], fn line, acc -> [line |> String.last() | acc] end)
      |> Enum.reverse()
      |> Enum.join()

    top = lines |> List.first()
    bottom = lines |> List.last()

    [top, right, bottom, left]
  end

  def count_matching_edges(tiles) do
    tiles
    |> Enum.reduce(Map.new(), fn {num, edges}, acc ->
      possibilities =
        edges
        |> Enum.concat(
          edges
          |> Enum.map(&String.reverse/1)
        )

      match_count =
        tiles
        |> Enum.reject(fn {num_check, _edges} -> num_check == num end)
        |> Enum.map(fn {_num, check_edges} ->
          check_edges
          |> Enum.count(fn edge -> possibilities |> Enum.member?(edge) end)
        end)
        |> Enum.sum()

      acc |> Map.put(num, match_count)
    end)
  end

  def transforms(grid) do
    0..3
    |> Enum.reduce([grid], fn _x, acc ->
      [last | _rest] = acc
      [last |> flip(), last |> flip() |> rotate()] |> Enum.concat(acc)
    end)
  end

  def find_edge_match(edge, side, tiles) do
    tiles
    |> Enum.find_value(fn {num, curr_tile} ->
      transforms = curr_tile |> transforms()

      match =
        transforms
        |> Enum.find_value(fn check_grid ->
          check_edge = check_grid |> get_edges() |> Enum.at(side)

          if check_edge == edge, do: check_grid
        end)

      # stated edge match found, return number with transformed tile
      if match, do: {num, match}
    end)
  end

  def get_first_row(grid, tiles, width) when length(grid) == 1 do
    {tile_num, tile} = grid |> List.first()
    tile_checks = tile |> transforms()

    [first_tile, next] =
      tile_checks
      |> Enum.find_value(fn check_tile ->
        # check right edge of tile permutation
        tile_match = check_tile |> get_edges() |> Enum.at(1) |> find_edge_match(3, tiles)

        if tile_match, do: [{tile_num, check_tile}, tile_match]
      end)

    new_grid = [first_tile, next]

    tiles_left =
      tiles
      |> Enum.reject(fn {num, _} ->
        {next_num, _} = next
        num == next_num
      end)

    assemble_grid(
      new_grid,
      tiles_left,
      width
    )
  end

  def get_first_row(grid, tiles, width) do
    {_last_num, tile} = grid |> List.last()

    IO.inspect(tiles)
    # right edge of last tile
    edge = tile |> get_edges() |> Enum.at(1)

    {next_num, next_tile} = edge |> find_edge_match(3, tiles)

    new_grid = grid |> Enum.concat([{next_num, next_tile}])

    tiles_left =
      tiles
      |> Enum.reject(fn {num, _} ->
        num == next_num
      end)

    get_first_row(
      new_grid,
      tiles_left,
      width
    )
  end

  def assemble_grid(grid, tiles, grid_width) when length(grid) < grid_width,
    do: get_first_row(grid, tiles, grid_width)

  def assemble_grid(grid, tiles, grid_width) do
    grid
  end

  def main do
    tiles = get_input()
    grid_width = :math.sqrt(tiles |> Enum.count()) |> round()

    edges = tiles |> Enum.map(fn {num, tile} -> {num, tile |> get_edges()} end)

    matches =
      edges
      |> count_matching_edges()

    part1_answer =
      matches
      |> Enum.reduce(1, fn {num, count}, acc -> if count == 2, do: acc * num, else: acc end)

    {starting_corner, _count} = matches |> Enum.find(fn {_num, count} -> count == 2 end)

    # find starting corner and pass in other edges

    starting_grid = [
      tiles
      |> Enum.find(fn {num, _edges} -> num == starting_corner end)
    ]

    starting_grid
    |> assemble_grid(
      tiles |> Enum.reject(fn {num, _edges} -> num == starting_corner end),
      grid_width
    )

    {part1_answer}
  end
end

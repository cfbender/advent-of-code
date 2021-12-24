defmodule AdventOfCode.Day23 do
  # credit to https://github.com/mathsaey/adventofcode/blob/master/lib/2021/23.ex for the guidance

  # okay, using a library finally because sorting the queue is just too slow
  alias PriorityQueue

  @room_xs 2..8//2

  def hallway_data(max_y \\ 2) do
    hallway = for x <- 0..10, do: {x, max_y}
    hallway_spots = Enum.reject(hallway, fn {x, _} -> x in @room_xs end)

    {hallway, hallway_spots}
  end

  def parse_input(input, max_y \\ 2) do
    [_, _ | rest] = input

    rooms =
      Enum.reverse(rest)
      |> Enum.filter(fn line -> not Regex.match?(~r/^#+$/, line) end)
      |> Enum.with_index()
      |> Enum.map(fn {line, y} ->
        String.codepoints(line)
        |> Enum.filter(&(&1 != "#"))
        |> Enum.map(&String.downcase/1)
        |> Enum.map(&String.to_atom/1)
        |> then(fn pods -> for(x <- @room_xs, do: {x, y}) |> Enum.zip(pods) end)
      end)

    {hallway, _} = hallway_data(max_y)

    Enum.concat(
      Enum.map(hallway, &{&1, nil}),
      rooms
    )
    |> List.flatten()
    |> Map.new()
  end

  def a_star(map, max_y \\ 2) do
    PriorityQueue.new()
    |> PriorityQueue.push({0, map}, 0)
    |> a_star(%{map => 0}, max_y)
  end

  def a_star(queue, costs, max_y) do
    {{:value, {curr_cost, curr_map}}, rest_queue} = PriorityQueue.pop(queue)

    if reached_goal?(curr_map, max_y) do
      curr_cost
    else
      # add maps after possible moves with cost to queue
      {new_queue, new_costs} =
        valid_moves({curr_map, max_y})
        |> Enum.map(&{move(curr_map, &1), curr_cost + cost(&1)})
        |> Enum.reduce({rest_queue, costs}, &a_star_grade_state(&1, &2, max_y))

      a_star(new_queue, new_costs, max_y)
    end
  end

  def a_star_grade_state({new_state, cost}, {queue, costs}, max_y) do
    if not Map.has_key?(costs, new_state) or cost < costs[new_state] do
      h_score = cost + lowest_possible_goal_cost(new_state, max_y)
      {PriorityQueue.push(queue, {cost, new_state}, h_score), Map.put(costs, new_state, cost)}
    else
      {queue, costs}
    end
  end

  def lowest_possible_goal_cost(map, max_y) do
    map
    |> Enum.filter(&in_hallway?(&1, max_y))
    |> Enum.reject(fn {_, v} -> is_nil(v) end)
    # find cost to enter correct room
    |> Enum.map(fn {location, pod} -> {location, {room(pod), max_y - 1}, pod} end)
    |> Enum.map(&cost/1)
    |> Enum.sum()
  end

  def reached_goal?(map, max_y) do
    map
    |> Enum.any?(fn
      # any in hallway
      {{_, y}, pod} when y == max_y and not is_nil(pod) -> true
      # any in mismatched room
      {{x, y}, pod} when y < max_y -> pod && x != room(pod)
      _ -> false
    end)
    |> Kernel.not()
  end

  # returns list of tuples of {from, to, type}
  def valid_moves(state = {map, max_y}) do
    {hallway_pods, room_pods} =
      map
      |> Enum.reject(fn {_, v} -> is_nil(v) end)
      |> Enum.reject(&in_final_position?(state, &1))
      |> Enum.split_with(&in_hallway?(&1, max_y))

    {_, hallway_spots} = hallway_data(max_y)

    Enum.concat(
      Enum.map(hallway_pods, fn {c, p} -> {c, final_pos(state, p), p} end),
      Enum.flat_map(room_pods, fn {c, p} -> Enum.map(hallway_spots, &{c, &1, p}) end)
    )
    |> Enum.filter(&has_path?(state, &1))
  end

  # look in correct room where first spot not occupied by correct pod is
  def final_pos({map, max_y}, pod),
    do: {room(pod), Enum.find(0..(max_y - 1), &(map[{room(pod), &1}] != pod))}

  # correct room for each type of pod
  def room(:a), do: 2
  def room(:b), do: 4
  def room(:c), do: 6
  def room(:d), do: 8

  def move(map, {{x_from, y_from}, {x_to, y_to}, p}) do
    map
    |> Map.put({x_from, y_from}, nil)
    |> Map.put({x_to, y_to}, p)
  end

  def moves({x_from, y_from}, {x_to, y_to}), do: abs(x_from - x_to) + abs(y_to - y_from)

  def cost({from, to, :a}), do: moves(from, to)
  def cost({from, to, :b}), do: moves(from, to) * 10
  def cost({from, to, :c}), do: moves(from, to) * 100
  def cost({from, to, :d}), do: moves(from, to) * 1000

  def in_hallway?({{_x, y}, _v}, max_y), do: y == max_y

  def has_path?(state, {start, stop, _}), do: path(state, start, stop) |> Enum.all?(&is_nil/1)

  # get path of moves by walking through map
  def path({map, max_y}, {x_from, y_from}, {x_to, y_to}) do
    Enum.concat(
      # all hallway
      for(x <- x_from..x_to, do: {x, max_y}),
      # all y paths, either entering or exiting room
      for(y <- y_from..y_to, do: {if(y_from < y_to, do: x_from, else: x_to), y})
    )
    |> List.delete({x_from, y_from})
    |> Enum.map(&map[&1])
  end

  def in_final_position?({_, max_y}, {{_, y}, _}) when y == max_y, do: false

  def in_final_position?({map, max_y}, {{x, y}, pod}) do
    # look down from current y into room and see if all pods in front are correct
    room(pod) == x and Enum.all?(y..0, &(map[{x, &1}] == pod))
  end

  def part1(input) do
    input
    |> parse_input()
    |> a_star()
  end

  def part2(input) do
    [top, hallway, front, back, bottom] = input

    [top, hallway, front, "#D#C#B#A#", "#D#B#A#C#", back, bottom]
    |> parse_input(4)
    |> a_star(4)
  end
end

defmodule AdventOfCode.Day16 do
  @moduledoc """
  Man, this one was brutal. I mean you can tell that just from the completion statistics at the time of writing this.

  I could not have done it without the following comment:
  https://www.reddit.com/r/adventofcode/comments/zn6k1l/comment/j0h0qcr/

  You'll notice I implemented that solution almost exactly here, and the heuristics were incredibly helpful to shorten part 2.
  I did a little refactoring to reduce the code duplication, but it may have made it less readable because of all the ternary
  operations I had to do to change some state values when it was an elephant vs human moving.

  Hopefully this is the hardest one, because this is probably the hardest problem I've done since starting in 2020 (maybe this or 2020 day 20)!
  """
  alias AdventOfCode.Helpers.Graph
  alias Prioqueue

  import AdventOfCode.Helpers
  require Record

  @source {"AA", 0}

  Record.defrecord(:state,
    valve: @source,
    elephant_valve: @source,
    in_time: 0,
    elephant_in_time: 4,
    limit: 30,
    closed: MapSet.new(),
    pressure: 0
  )

  # inverted to use a maximum queue
  def compare_records(state(pressure: pressure_a), state(pressure: pressure_b)) do
    cond do
      pressure_a < pressure_b -> :gt
      pressure_a > pressure_b -> :lt
      pressure_a == pressure_b -> :eq
    end
  end

  def parse_input(input) do
    valves =
      input
      |> lines()
      |> Enum.reduce(%{}, fn line, acc ->
        [valve, flow_rate, next] =
          Regex.run(~r/Valve (.*) has flow rate=(.*); tunnels? leads? to valves? (.*)/, line)
          |> Enum.drop(1)

        next = String.split(next, ", ", trim: true)

        Map.put(acc, valve, %{flow_rate: String.to_integer(flow_rate), next: next})
      end)

    Enum.reduce(valves, Graph.new(), fn {valve, %{flow_rate: flow_rate, next: next}}, graph ->
      Graph.add_vertex(graph, {valve, flow_rate})
      |> then(fn g ->
        edges =
          Enum.map(next, fn n_valve ->
            [{valve, flow_rate}, {n_valve, valves[n_valve][:flow_rate]}]
          end)

        Graph.add_edges(g, edges)
      end)
    end)
  end

  def all_with_flow(graph, remove \\ MapSet.new()),
    do: MapSet.reject(graph.vertices, fn {_, flow} -> flow == 0 end) |> MapSet.difference(remove)

  def move_elephant(graph, dist, states) do
    Enum.reduce(states, [], fn curr_state, acc ->
      move(graph, dist, curr_state, true) ++ acc
    end)
  end

  def move(
        graph,
        dist,
        state(
          valve: valve,
          elephant_valve: elephant_valve,
          in_time: in_time,
          elephant_in_time: elephant_in_time,
          limit: limit,
          closed: closed,
          pressure: pressure
        ) = curr_state,
        elephant \\ false
      ) do
    time_check = if elephant, do: elephant_in_time, else: in_time

    # skip if the one we are checking isn't the minimum time for that state
    # ie. if the elephant is behind the human in that state, return it to be updated later
    if min(in_time, elephant_in_time) == time_check do
      curr_location = if elephant, do: elephant_valve, else: valve

      if MapSet.member?(closed, curr_location) do
        pressure_time = if elephant, do: elephant_in_time, else: in_time
        {_, flow} = curr_location
        # at a closed valve, open it!
        new_pressure = pressure + (limit - 1 - pressure_time) * flow

        [
          state(curr_state,
            # one minute to open
            in_time: if(not elephant, do: in_time + 1, else: in_time),
            elephant_in_time: if(elephant, do: elephant_in_time + 1, else: elephant_in_time),
            pressure: new_pressure,
            closed: MapSet.difference(closed, MapSet.new([curr_location]))
          )
        ]
      else
        # move to all open valves
        open = MapSet.difference(graph.vertices, closed)
        open_valves_with_flow = all_with_flow(graph, open)

        new_states =
          Enum.reduce(open_valves_with_flow, [], fn dest, acc ->
            time_to_move = dist[{curr_location, dest}]

            # set destination to next open valve, and update time with precomputed shortest path
            new_state =
              state(curr_state,
                valve: if(not elephant, do: dest, else: valve),
                elephant_valve: if(elephant, do: dest, else: elephant_valve),
                in_time: if(not elephant, do: in_time + time_to_move, else: in_time),
                elephant_in_time:
                  if(elephant, do: elephant_in_time + time_to_move, else: elephant_in_time)
              )

            [new_state | acc]
          end)

        if length(new_states) == 0,
          # short circuit to completed if nothing left to do
          do: [
            state(curr_state,
              in_time: if(not elephant, do: limit, else: in_time),
              elephant_in_time: if(elephant, do: limit, else: elephant_in_time)
            )
          ],
          else: new_states
      end
    else
      [curr_state]
    end
  end

  def most_pressure(graph, queue, dist, best_for_time \\ %{}, elephant \\ false, best \\ 0) do
    case Prioqueue.extract_min(queue) do
      {:error, :empty} ->
        best

      {:ok,
       {state(
          pressure: pressure,
          in_time: in_time,
          limit: limit,
          elephant_in_time: elephant_in_time,
          closed: closed
        ) = curr_state, rest_queue}} ->
        new_best = max(pressure, best)
        curr_time = min(in_time, elephant_in_time)
        curr_best_for_time = Map.get(best_for_time, curr_time, 0)
        new_best_for_time = Map.put(best_for_time, curr_time, max(curr_best_for_time, pressure))

        # Drop a path if it's less than 80% the current best for that time
        p_heuristic = if elephant, do: 1.25 * pressure, else: 1.5 * pressure
        # Only start doing this after t=10 and t=12 for part 1 and 2 respectively.
        t_heuristic = if elephant, do: 12, else: 10

        # only proceed if someone has time left and there are open valves
        # use heuristics to short-circuit paths that won't beat the current bests
        if curr_time < limit and MapSet.size(closed) > 0 and
             (curr_time < t_heuristic or p_heuristic >= curr_best_for_time) do
          new_states =
            move(graph, dist, curr_state)
            |> then(fn states ->
              if elephant do
                move_elephant(graph, dist, states)
              else
                states
              end
            end)

          new_queue = Enum.reduce(new_states, rest_queue, &Prioqueue.insert(&2, &1))

          most_pressure(graph, new_queue, dist, new_best_for_time, elephant, new_best)
        else
          most_pressure(graph, rest_queue, dist, new_best_for_time, elephant, new_best)
        end
    end
  end

  def part1(input) do
    distances =
      input
      |> Graph.floyd_warshall(true)

    closed = all_with_flow(input)

    queue =
      Prioqueue.new([state(closed: closed, elephant_in_time: 30)], cmp_fun: &compare_records/2)

    most_pressure(input, queue, distances)
  end

  def part2(input) do
    distances =
      input
      |> Graph.floyd_warshall(true)

    closed = all_with_flow(input)
    queue = Prioqueue.new([state(in_time: 4, closed: closed)], cmp_fun: &compare_records/2)

    most_pressure(input, queue, distances, %{}, true)
  end
end

defmodule AdventOfCode.Day16 do
  @moduledoc """
  """
  alias AdventOfCode.Helpers.Graph
  import AdventOfCode.Helpers

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

  @source {"AA", 0}

  def most_pressure(
        graph,
        limit,
        start \\ @source,
        curr_time \\ 0,
        open \\ MapSet.new(),
        pressure \\ 0
      )

  def most_pressure(graph, limit, {_, flow} = start, curr_time, open, pressure) do
    valves_with_flow = graph.vertices |> MapSet.reject(fn {_, flow} -> flow == 0 end)

    if curr_time >= limit or MapSet.difference(valves_with_flow, open) |> MapSet.size() == 0 do
      pressure |> dbg()
    else
      {new_time, new_open} =
        if MapSet.member?(open, start) or flow == 0 do
          # one minute to move
          {curr_time + 1, open}
        else
          # one minute to move, one minute to open
          {curr_time + 2, MapSet.put(open, start)}
        end

      new_pressure = pressure + flow * (limit - 1 - new_time)
      neighbors = Graph.neighbors(graph, start)

      Enum.max_by(neighbors, fn neighbor ->
        most_pressure(graph, limit, neighbor, new_time, new_open, new_pressure)
      end)
    end
  end

  def part1(input) do
    input
    |> most_pressure(30)
  end

  def part2(_args) do
  end
end

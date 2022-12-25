defmodule AdventOfCode.Day19 do
  @moduledoc """
  Just day 16, but with harder heuristics to figure out. There's a decent chance this wouldn't work for any input.
  It is very slow though, and under-optimized (around a minute on my mac)

  Just wanted to get it over with, so here's this mess.
  """
  import AdventOfCode.Helpers
  alias Prioqueue

  defmodule Blueprint do
    defstruct number: 0, ore: 0, clay: 0, obsidian: {0, 0}, geode: {0, 0}
  end

  def initial_state() do
    %{
      ore: 0,
      clay: 0,
      obsidian: 0,
      geode: 0,
      ore_robot: 1,
      clay_robot: 0,
      obsidian_robot: 0,
      geode_robot: 0,
      time: 0
    }
  end

  # inverted to use a maximum queue
  def compare_states(%{geode: geode_a}, %{geode: geode_b}) do
    cond do
      geode_a < geode_b -> :gt
      geode_a > geode_b -> :lt
      geode_a == geode_b -> :eq
    end
  end

  def parse_input(input) do
    input
    |> lines()
    |> Enum.map(fn line ->
      [num, ore, clay, obs_ore, obs_clay, geode_ore, geode_obs] =
        Regex.scan(~r/\d+/, line)
        |> List.flatten()
        |> Enum.map(&String.to_integer/1)

      %Blueprint{
        number: num,
        ore: ore,
        clay: clay,
        obsidian: {obs_ore, obs_clay},
        geode: {geode_ore, geode_obs}
      }
    end)
  end

  def can_build?(
        %Blueprint{geode: {ore_cost, obisidian_cost}},
        %{ore: ore, obsidian: obsidian},
        :geode_robot
      ),
      do: ore >= ore_cost and obsidian >= obisidian_cost

  def can_build?(
        %Blueprint{obsidian: {ore_cost, clay_cost}},
        %{ore: ore, clay: clay},
        :obsidian_robot
      ),
      do: ore >= ore_cost and clay >= clay_cost

  def can_build?(%Blueprint{clay: ore_cost}, %{ore: ore}, :clay_robot),
    do: ore >= ore_cost

  def can_build?(%Blueprint{ore: ore_cost}, %{ore: ore}, :ore_robot),
    do: ore >= ore_cost

  def build(%Blueprint{geode: {g_ore, g_obs}}, :geode_robot) do
    fn state ->
      %{
        state
        | ore: state.ore - g_ore,
          obsidian: state.obsidian - g_obs,
          geode_robot: state.geode_robot + 1
      }
    end
  end

  def build(%Blueprint{obsidian: {obs_ore, obs_clay}}, :obsidian_robot) do
    fn state ->
      %{
        state
        | ore: state.ore - obs_ore,
          clay: state.clay - obs_clay,
          obsidian_robot: state.obsidian_robot + 1
      }
    end
  end

  def build(blueprint, :clay_robot) do
    fn state -> %{state | ore: state.ore - blueprint.clay, clay_robot: state.clay_robot + 1} end
  end

  def build(blueprint, :ore_robot) do
    fn state -> %{state | ore: state.ore - blueprint.ore, ore_robot: state.ore_robot + 1} end
  end

  def try_builds(
        state,
        blueprint,
        limits
      ) do
    # start with list that skips the build
    start = [fn state -> state end]

    cond do
      can_build?(blueprint, state, :geode_robot) ->
        [build(blueprint, :geode_robot)]

      true ->
        Enum.reduce([:obsidian_robot, :clay_robot, :ore_robot], start, fn type, builds ->
          if can_build?(blueprint, state, type) and state[type] < limits[type] do
            [build(blueprint, type) | builds]
          else
            builds
          end
        end)
    end
  end

  def update_state(
        %{
          ore: ore,
          clay: clay,
          obsidian: obsidian,
          geode: geode,
          ore_robot: ore_robot,
          clay_robot: clay_robot,
          obsidian_robot: obsidian_robot,
          geode_robot: geode_robot,
          time: time
        } = state
      ) do
    %{
      state
      | ore: ore + ore_robot,
        clay: clay + clay_robot,
        obsidian: obsidian + obsidian_robot,
        geode: geode + geode_robot,
        time: time + 1
    }
  end

  def find_best(
        blueprint,
        queue,
        time_limit,
        limits,
        best \\ 0,
        best_robot_for_time \\ %{},
        seen \\ MapSet.new()
      ) do
    case Prioqueue.extract_min(queue) do
      {:error, :empty} ->
        best

      {:ok, {%{time: time} = curr_state, rest_queue}} ->
        new_best = max(curr_state.geode, best)
        new_seen = MapSet.put(seen, curr_state)

        new_best_robot_for_time =
          Map.put(best_robot_for_time, curr_state.time, curr_state.geode_robot)

        time_left = time_limit - curr_state.time
        # all possible geodes if you built a robot every minute regardless of resources
        max_geodes = Enum.sum(1..time_left)
        # all possible geode upper bound
        g_heuristic = curr_state.geode + curr_state.geode_robot * time_left + max_geodes
        # drop if you are two robots behind the best for the time
        should_drop =
          Map.get(best_robot_for_time, curr_state.time, 0) >= curr_state.geode_robot + 2

        if time < time_limit and g_heuristic > best and not MapSet.member?(seen, curr_state) and
             not should_drop do
          # try builds with the starting state inventory of mats
          builds = try_builds(curr_state, blueprint, limits)

          # update the starting state for current robots and increment time
          new_state = update_state(curr_state)

          new_queue =
            Enum.reduce(builds, rest_queue, fn build, q ->
              # call the build callback to update inventory based on what you could build originally, including not building
              Prioqueue.insert(q, new_state |> build.())
            end)

          find_best(
            blueprint,
            new_queue,
            time_limit,
            limits,
            new_best,
            new_best_robot_for_time,
            new_seen
          )
        else
          find_best(
            blueprint,
            rest_queue,
            time_limit,
            limits,
            new_best,
            new_best_robot_for_time,
            new_seen
          )
        end
    end
  end

  @doc """
  Limit the robots to the consumption cap of other robots
  """
  def robot_limits(%Blueprint{
        ore: ore,
        clay: c_ore,
        obsidian: {o_ore, o_clay},
        geode: {g_ore, g_obs}
      }) do
    %{
      ore_robot: Enum.max([ore, c_ore, o_ore, g_ore]),
      clay_robot: o_clay,
      obsidian_robot: g_obs
    }
  end

  def part1(input) do
    queue = Prioqueue.new([initial_state()], cmp_fun: &compare_states/2)

    Enum.map(input, fn %{number: n} = bp ->
      limits = robot_limits(bp)

      Task.async(fn ->
        n * find_best(bp, queue, 24, limits)
      end)
    end)
    |> Task.await_many(:infinity)
    |> Enum.sum()
  end

  def part2(input) do
    queue = Prioqueue.new([initial_state()], cmp_fun: &compare_states/2)

    Enum.take(input, 3)
    |> Enum.map(fn bp ->
      limits = robot_limits(bp)

      Task.async(fn ->
        find_best(bp, queue, 32, limits)
      end)
    end)
    |> Task.await_many(:infinity)
    |> Enum.product()
  end
end

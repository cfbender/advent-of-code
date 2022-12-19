defmodule AdventOfCode.Day19 do
  @moduledoc """
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
        :geode
      ),
      do: ore >= ore_cost and obsidian >= obisidian_cost

  def can_build?(%Blueprint{obsidian: {ore_cost, clay_cost}}, %{ore: ore, clay: clay}, :obsidian),
    do: ore >= ore_cost and clay >= clay_cost

  def can_build?(%Blueprint{clay: ore_cost}, %{ore: ore}, :clay),
    do: ore >= ore_cost

  def can_build?(%Blueprint{ore: ore_cost}, %{ore: ore}, :ore),
    do: ore >= ore_cost

  def build(%Blueprint{geode: {g_ore, g_obs}}, :geode) do
    fn state ->
      %{
        state
        | ore: state.ore - g_ore,
          obsidian: state.obsidian - g_obs,
          geode_robot: state.geode_robot + 1
      }
    end
  end

  def build(%Blueprint{obsidian: {obs_ore, obs_clay}}, :obsidian) do
    fn state ->
      %{
        state
        | ore: state.ore - obs_ore,
          clay: state.clay - obs_clay,
          obsidian_robot: state.obsidian_robot + 1
      }
    end
  end

  def build(blueprint, :clay) do
    fn state -> %{state | ore: state.ore - blueprint.clay, clay_robot: state.clay_robot + 1} end
  end

  def build(blueprint, :ore) do
    fn state -> %{state | ore: state.ore - blueprint.ore, ore_robot: state.ore_robot + 1} end
  end

  def try_builds(
        state,
        blueprint
      ) do
    # start with list that skips the build
    Enum.reduce([:geode, :obsidian, :clay, :ore], [fn state -> state end], fn type, builds ->
      if can_build?(blueprint, state, type) do
        [build(blueprint, type) | builds]
      else
        builds
      end
    end)
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

  def find_best(blueprint, queue, best \\ 0) do
    case Prioqueue.extract_min(queue) do
      {:error, :empty} ->
        best

      {:ok, {%{time: time} = curr_state, rest_queue}} ->
        new_best = max(curr_state.geode, best)
        # should_drop = time > 20 and curr_state.geode == 0

        if time < 24 do
          # try builds with the starting state inventory of mats
          builds = try_builds(curr_state, blueprint)

          # update the starting state for current robots and increment time
          new_state = update_state(curr_state)

          new_queue =
            Enum.reduce(builds, rest_queue, fn build, q ->
              # call the build callback to update inventory based on what you could build originally, including not building
              Prioqueue.insert(q, new_state |> build.())
            end)

          find_best(blueprint, new_queue, new_best)
        else
          find_best(blueprint, rest_queue, new_best)
        end
    end
  end

  def part1(input) do
    # 1197 too low
    queue = Prioqueue.new([initial_state()], cmp_fun: &compare_states/2)

    Enum.map(input, fn %{number: n} = bp ->
      Task.async(fn ->
        dbg(n * find_best(bp, queue))
      end)
    end)
    |> Task.await_many(:infinity)
    |> Enum.sum()
  end

  def part2(_args) do
  end
end

defmodule AdventOfCode.Day07 do
  @moduledoc """
  WOW what a ramp up. I really got stuck on this one, thinking how it would be really hard to do this without mutability.
  I eventually got that in add/3, but then got _really_ stuck on some stupid bugs with my cd/2 implementation
  as well as not understanding the problem well enough (getting individual dir sizes)

  In hindsight this is incredibly overcomplicated. All I needed to do was build a map of paths and their sizes.
  Lesson learned to fully think about the problem before starting an implementation

  There's also like zero tail call recursion in this, but at this point I don't want to mess with it anymore lol
  """
  def parse_input(input) do
    input
    |> String.split("\n", trim: true)
  end

  def cwd_to_path(cwd), do: ["/" | String.split(cwd, "/", trim: true)]

  # state shape: %{"/" => [%{size: 14848514, name: "b.txt"}, %{"a" => [%{size: 29116, name: "f"}]}]}
  def add(state, addition, [to_dir]) do
    Map.update!(state, to_dir, fn list -> [addition | list] end)
  end

  def add(state, addition, [first, next | path]) do
    Map.update!(state, first, fn list ->
      Enum.map(list, fn item ->
        if Map.has_key?(item, next) do
          add(item, addition, [next | path])
        else
          item
        end
      end)
    end)
  end

  def add_dir(state, dir, cwd), do: add(state, %{dir => [], type: :dir}, cwd_to_path(cwd))
  def add_file(state, file, cwd), do: add(state, file, cwd_to_path(cwd))

  def ls(input, cwd, state) do
    {output, rest} = Enum.split_while(input, fn line -> not String.starts_with?(line, "$") end)

    state =
      Enum.reduce(output, state, fn line, acc ->
        [type_or_size, name] = String.split(line)

        case type_or_size do
          "dir" ->
            add_dir(acc, name, cwd)

          _ ->
            add_file(acc, %{size: String.to_integer(type_or_size), name: name, type: :file}, cwd)
        end
      end)

    {state, rest}
  end

  def cd(cwd, cmd) do
    case cmd do
      "/" ->
        "/"

      ".." ->
        path = "/#{String.split(cwd, "/", trim: true) |> Enum.drop(-1) |> Enum.join("/")}/"
        if String.length(path) == 0, do: "/", else: path

      _ ->
        cwd <> cmd <> "/"
    end
  end

  def get_structure([], _cwd, state), do: state
  # shape:
  # dir: %{ [dir_name] => [files and dirs], type: dir}
  # file: %{size: <size>, name: <name>, type: file}
  def get_structure(input, cwd, state) do
    [curr | rest] = input

    case curr do
      "$ cd " <> cmd ->
        cwd = cd(cwd, cmd)
        get_structure(rest, cwd, state)

      "$ ls" ->
        {state, rest} = ls(rest, cwd, state)
        get_structure(rest, cwd, state)
    end
  end

  def get_dir_name(dir),
    do:
      Map.keys(dir)
      |> Enum.filter(fn key ->
        is_binary(key)
      end)
      |> List.first()

  def dir_size(state, cwd) do
    {files, dirs} = Enum.split_with(state[cwd], fn item -> item.type == :file end)
    file_size = Stream.map(files, fn file -> file.size end) |> Enum.sum()

    dir_size =
      Enum.reduce(dirs, 0, fn dir, acc ->
        name = get_dir_name(dir)
        size = dir_size(dir, name)
        acc + size
      end)

    dir_size + file_size
  end

  def dir_sizes(state, cwd, sizes \\ []) do
    sizes = [dir_size(state, cwd) | sizes]
    dir = state[cwd]

    children = Enum.filter(dir, fn item -> item.type == :dir end)

    [
      Enum.map(children, fn dir ->
        name = get_dir_name(dir)
        dir_sizes(dir, name)
      end)
      | sizes
    ]
    |> List.flatten()
  end

  def part1(input) do
    input
    |> get_structure("/", %{"/" => []})
    |> dir_sizes("/")
    |> Enum.filter(fn size -> size <= 100_000 end)
    |> Enum.sum()
  end

  def part2(input) do
    fs =
      input
      |> get_structure("/", %{"/" => []})

    empty_space = 70_000_000 - dir_size(fs, "/")
    needed = 30_000_000 - empty_space

    dir_sizes(fs, "/")
    |> Enum.sort()
    |> Enum.find(fn dir_size -> dir_size >= needed end)
  end
end

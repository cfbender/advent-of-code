use Bitwise

defmodule Aoc do
  def get_input do
    {:ok, input} = File.read("../input.txt")
    input |> String.split("\n", trim: true)
  end

  def get_mem_ins(ins) do
    [_, addr, value] = Regex.run(~r/mem\[(\d*)\] = (\d*)/, ins)
    {addr |> String.to_integer(), value |> String.to_integer()}
  end

  def mask_bits(mask, value) do
    and_mask = mask |> String.replace("X", "1") |> String.to_integer(2)
    or_mask = mask |> String.replace("X", "0") |> String.to_integer(2)
    (value &&& and_mask) ||| or_mask
  end

  def permutations([], _k), do: [[]]
  def permutations(_list, 0), do: [[]]

  def permutations(list, k),
    do: for(elem <- list, rest <- permutations(list, k - 1), do: [elem | rest])

  def replace_x([], mask), do: mask

  def replace_x(list, mask) do
    [head | rest] = list
    new_mask = mask |> String.replace("X", head |> Integer.to_string(), global: false)
    replace_x(rest, new_mask)
  end

  def get_floating_writes(mask, addr) do
    or_result =
      mask
      |> String.replace("X", "0")
      |> String.to_integer(2) |||
        addr

    new_mask =
      or_result
      |> Integer.to_string(2)
      |> String.pad_leading(36, "0")
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.map_join("", fn {x, idx} ->
        if mask |> String.at(idx) == "X", do: "X", else: x
      end)

    num_x = mask |> String.graphemes() |> Enum.count(&(&1 == "X"))
    perms = permutations([0, 1], num_x)

    perms |> Enum.map(fn perm -> replace_x(perm, new_mask) |> String.to_integer(2) end)
  end

  def part(mem, [], _part) do
    mem |> Map.delete(:mask) |> Map.values() |> Enum.sum()
  end

  def part(mem, ins, part) do
    curr_ins = ins |> Enum.at(0)

    cond do
      curr_ins |> String.starts_with?("mask") ->
        "mask = " <> mask = curr_ins
        part(mem |> Map.put(:mask, mask), ins |> List.delete_at(0), part)

      true ->
        {addr, value} = curr_ins |> get_mem_ins()
        mask = mem |> Map.get(:mask)

        case part do
          1 ->
            masked_value = mask |> mask_bits(value)

            part(
              mem |> Map.put(addr, masked_value),
              ins |> List.delete_at(0),
              part
            )

          2 ->
            addrs = get_floating_writes(mask, addr)

            new_mem =
              addrs
              |> Enum.reduce(mem, fn x, acc ->
                acc |> Map.put(x, value)
              end)

            part(new_mem, ins |> List.delete_at(0), part)
        end
    end
  end

  def main do
    input = get_input()
    memory = Map.new()

    part1 = part(memory, input, 1)
    part2 = part(memory, input, 2)
    {part1, part2}
  end
end

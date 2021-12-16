defmodule AdventOfCode.Day16 do
  def hex_to_binary(hex_string) do
    String.codepoints(hex_string)
    |> Enum.map(fn x ->
      {decimal, _} = Integer.parse(x, 16)

      decimal |> Integer.to_string(2) |> String.pad_leading(4, "0")
    end)
    |> Enum.join()
  end

  def parse_literal(packet) do
    full_packet =
      String.codepoints(packet)
      |> Enum.chunk_every(5)

    Enum.reduce_while(full_packet, [], fn chunk, acc ->
      [prefix | num] = chunk

      if prefix == "1" do
        {:cont, [num | acc]}
      else
        full_digits = [num | acc] |> Enum.reverse()
        decimal = List.flatten(full_digits) |> Enum.join() |> Integer.parse(2) |> elem(0)

        rest_digits =
          Enum.take(full_packet, -(length(full_packet) - length(full_digits)))
          |> List.flatten()
          |> Enum.join()

        {:halt, {decimal, rest_digits}}
      end
    end)
  end

  def parse_operator(packet, type) do
    if type == "0" do
      <<sub_length_bin::binary-size(15)>> <> sub_packets = packet
      sub_length = Integer.parse(sub_length_bin, 2) |> elem(0)

      <<packets::binary-size(sub_length)>> <> rest = sub_packets
      child_data = parse_packets(packets)

      {child_data, rest}
    else
      <<sub_packet_count_bin::binary-size(11)>> <> sub_packets = packet
      sub_packet_count = Integer.parse(sub_packet_count_bin, 2) |> elem(0)
      parse_packets(sub_packets, [], true, sub_packet_count)
    end
  end

  def parse_packets(input, packet_data \\ [], limit? \\ false, count \\ 0, current \\ 0) do
    if limit? and current == count do
      {packet_data |> Enum.reverse(), input}
    else
      if Enum.all?(String.codepoints(input), &(&1 == "0")) do
        packet_data |> Enum.reverse()
      else
        <<version::binary-size(3)>> <> <<id::binary-size(3)>> <> rest = input
        version_decimal = Integer.parse(version, 2) |> elem(0)
        id_decimal = Integer.parse(id, 2) |> elem(0)

        if id_decimal == 4 do
          {decimal, rest_packets} = parse_literal(rest)
          data = %{version: version_decimal, id: id_decimal, type: :literal, children: decimal}
          parse_packets(rest_packets, [data | packet_data], limit?, count, current + 1)
        else
          <<type::binary-size(1)>> <> rest_op = rest
          {child_data, after_op} = parse_operator(rest_op, type)

          data = %{
            version: version_decimal,
            id: id_decimal,
            type: :operator,
            children: child_data
          }

          parse_packets(after_op, [data | packet_data], limit?, count, current + 1)
        end
      end
    end
  end

  def sum_versions(packets, count \\ 0)
  def sum_versions([], count), do: count

  def sum_versions(packets, count) do
    top_level_sum = Enum.map(packets, fn packet -> packet[:version] end) |> Enum.sum()

    all_children =
      Enum.flat_map(packets, fn packet ->
        children = packet[:children]
        if is_list(children), do: children, else: []
      end)
      |> Enum.filter(&(not is_nil(&1)))

    sum_versions(all_children, top_level_sum + count)
  end

  def part1(input) do
    input
    |> hex_to_binary()
    |> parse_packets()
    |> sum_versions()
  end

  def part2(_args) do
  end
end

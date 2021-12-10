defmodule AdventOfCode.Day10 do
  @closers [")", "]", "}", ">"]

  @pairs %{"(" => ")", "[" => "]", "{" => "}", "<" => ">"}

  @corrupt_points %{")" => 3, "]" => 57, "}" => 1197, ">" => 25137}

  @complete_points %{")" => 1, "]" => 2, "}" => 3, ">" => 4}

  def parse_line(line) do
    String.codepoints(line)
    |> Enum.reduce_while([], fn curr, stack ->
      if Enum.member?(@closers, curr) do
        if Map.get(@pairs, List.first(stack)) == curr do
          # closed successfully
          [_prev | rest] = stack
          {:cont, rest}
        else
          {:halt, curr}
        end
      else
        {:cont, [curr | stack]}
      end
    end)
  end

  def part1(input) do
    input
    |> Enum.reduce(0, fn line, acc ->
      result = parse_line(line)

      if is_list(result) do
        acc
      else
        acc + @corrupt_points[result]
      end
    end)
  end

  def part2(input) do
    scores =
      input
      |> Enum.reduce([], fn line, acc ->
        result = parse_line(line)

        if is_list(result) do
          score =
            Enum.map(result, &Map.get(@pairs, &1))
            |> Enum.reduce(0, fn curr, total ->
              total * 5 + @complete_points[curr]
            end)

          [score | acc]
        else
          acc
        end
      end)
      |> Enum.sort()

    Enum.at(scores, floor(length(scores) / 2))
  end
end

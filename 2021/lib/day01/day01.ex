defmodule Aoc2021.Day01 do
  use Aoc2021

  def solve_a do
    depths = @puzzle_input |> File.stream!() |> Stream.map(&parse_line/1)
    initialDepth = depths |> Enum.take(1) |> List.first()

    depths
    |> Stream.drop(1)
    |> Enum.reduce({initialDepth, 0}, &increment_on_value_increase/2)
    |> elem(1)
  end

  def solve_b do
    chunk_size = 3
    chunk_shift_amount = 1

    depth_chunks =
      @puzzle_input
      |> File.stream!()
      |> Stream.map(&parse_line/1)
      |> Stream.chunk_every(chunk_size, chunk_shift_amount, :discard)

    initialSum = depth_chunks |> Enum.take(1) |> List.first() |> Enum.sum()

    reduceChunk = fn chunk, currentResult ->
      chunk |> Enum.sum() |> increment_on_value_increase(currentResult)
    end

    depth_chunks
    |> Stream.drop(1)
    |> Enum.reduce({initialSum, 0}, reduceChunk)
    |> elem(1)
  end

  defp parse_line(line), do: line |> String.trim() |> String.to_integer()

  defp increment_on_value_increase(nextValue, {previousValue, numberIncreases}) do
    {nextValue, if(nextValue > previousValue, do: numberIncreases + 1, else: numberIncreases)}
  end
end

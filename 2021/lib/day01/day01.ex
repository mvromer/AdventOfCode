defmodule Aoc2021.Day01 do
  use Aoc2021

  def solve_a do
    depths =
      File.stream!(@puzzle_input)
      |> Stream.map(&(&1 |> String.trim() |> String.to_integer()))

    initialDepth = depths |> Enum.take(1) |> List.first()

    depths
    |> Stream.drop(1)
    |> Enum.reduce({initialDepth, 0}, &increment_on_value_increase/2)
    |> elem(1)
  end

  def solve_b do
    depth_chunks =
      File.stream!(@puzzle_input)
      |> Stream.map(&(&1 |> String.trim() |> String.to_integer()))
      |> Stream.chunk_every(3, 1, :discard)

    initialSum = depth_chunks |> Enum.take(1) |> List.first() |> Enum.sum()

    reduceChunk = fn chunk, currentResult ->
      chunk |> Enum.sum() |> increment_on_value_increase(currentResult)
    end

    depth_chunks
    |> Stream.drop(1)
    |> Enum.reduce({initialSum, 0}, reduceChunk)
    |> elem(1)
  end

  defp increment_on_value_increase(nextValue, {previousValue, numberIncreases}) do
    {nextValue, if(nextValue > previousValue, do: numberIncreases + 1, else: numberIncreases)}
  end
end
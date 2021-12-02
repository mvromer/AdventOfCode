defmodule Aoc2021.Day01 do
  use Aoc2021

  def solve_a do
    depths = File.stream!(@puzzle_input)
    |> Stream.map(&(&1 |> String.trim |> String.to_integer))

    initialDepth = depths |> Enum.take(1) |> List.first

    depths |> Stream.drop(1)
    |> Enum.reduce({initialDepth, 0}, &increment_on_depth_increase/2)
    |> elem(1)
  end

  defp increment_on_depth_increase(nextDepth, {previousDepth, numberIncreases}) do
    {nextDepth, (if nextDepth > previousDepth, do: numberIncreases + 1, else: numberIncreases)}
  end
end

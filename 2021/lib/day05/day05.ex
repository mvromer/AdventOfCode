defmodule Aoc2021.Day05 do
  use Aoc2021

  def solve_a do
    is_horizontal_or_vertical_line = fn endpoints ->
      cond do
        endpoints.x1 == endpoints.x2 -> true
        endpoints.y1 == endpoints.y2 -> true
        true -> false
      end
    end

    @puzzle_input
    |> File.stream!()
    |> Stream.map(&parse_line/1)
    |> Stream.filter(is_horizontal_or_vertical_line)
    |> Stream.flat_map(&get_covered_locations/1)
    |> Enum.frequencies()
    |> Enum.filter(fn {_, count} -> count >= 2 end)
    |> Enum.count()
  end

  def solve_b do
    @puzzle_input
    |> File.stream!()
    |> Stream.map(&parse_line/1)
    |> Stream.flat_map(&get_covered_locations/1)
    |> Enum.frequencies()
    |> Enum.filter(fn {_, count} -> count >= 2 end)
    |> Enum.count()
  end

  defp parse_line(line) do
    pattern = ~r/(?<x1>\d+),(?<y1>\d+) -> (?<x2>\d+),(?<y2>\d+)/

    case Regex.named_captures(pattern, line) do
      %{"x1" => x1, "y1" => y1, "x2" => x2, "y2" => y2} ->
        # Remap keys to atoms for cleaner access throughout the rest of the code.
        %{
          x1: String.to_integer(x1),
          y1: String.to_integer(y1),
          x2: String.to_integer(x2),
          y2: String.to_integer(y2)
        }
    end
  end

  defp get_covered_locations(%{x1: x1, y1: y1, x2: x2, y2: y2}) do
    cond do
      x1 == x2 ->
        # Special case: vertical line.
        ys = if y1 <= y2, do: y1..y2, else: y2..y1
        ys |> Stream.map(fn y -> {x1, y} end)

      true ->
        slope = (y2 - y1) / (x2 - x1)
        xs = if x1 <= x2, do: x1..x2, else: x2..x1

        # In the case of horizontal/vertical lines and those whose slope is 1 (which the puzzle
        # guarantees), we can safely truncate the calcualted y-coordinate. This ensures all our
        # tuple elements are of the same data type (integers), which allows us to directly use
        # Enum.frequencies for grouping.
        xs |> Stream.map(fn x -> {x, trunc(y1 + slope * (x - x1))} end)
    end
  end
end

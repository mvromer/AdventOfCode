defmodule Aoc2021.Day02 do
  use Aoc2021

  def solve_a do
    move = fn next_move, {current_depth, current_horizontal} ->
      case next_move do
        {"forward", step} -> {current_depth, current_horizontal + step}
        {"up", step} -> {current_depth - step, current_horizontal}
        {"down", step} -> {current_depth + step, current_horizontal}
      end
    end

    {final_depth, final_horizontal} =
      @puzzle_input
      |> File.stream!()
      |> Stream.map(&parse_line/1)
      |> Enum.reduce({0, 0}, move)

    final_depth * final_horizontal
  end

  def solve_b do

  end

  defp parse_line(line) do
    pattern = ~r/(?<direction>forward|down|up) (?<step>\d+)/

    case Regex.named_captures(pattern, line) do
      %{"direction" => direction, "step" => step} -> {direction, String.to_integer(step)}
    end
  end
end

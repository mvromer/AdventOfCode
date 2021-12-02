defmodule Aoc2021 do
  defmacro __using__(_) do
    quote do
      @puzzle_input Path.dirname(__ENV__.file) |> Path.join("input.txt")
    end
  end
end

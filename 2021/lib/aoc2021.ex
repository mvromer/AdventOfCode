defmodule Aoc2021 do
  defmacro __using__(_) do
    quote do
      @puzzle_input Path.dirname(__ENV__.file) |> Path.join("input.txt")
      @example_input Path.dirname(__ENV__.file) |> Path.join("example.txt")
    end
  end
end

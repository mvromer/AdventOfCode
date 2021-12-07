defmodule Aoc2021.Day06 do
  use Aoc2021

  def solve_a do
    @puzzle_input
    |> File.stream!()
    |> Enum.flat_map(&parse_line/1)
    |> Stream.iterate(&advance_spawn_timers/1)
    |> Stream.drop(80)
    |> Stream.take(1)
    |> Enum.flat_map(&Function.identity/1)
    |> Enum.count()
  end

  def solve_b do
    # @example_input
    # |> File.stream!()
    # |> Enum.flat_map(&parse_line/1)
    # |> Enum.reduce(0, fn days_til_spawn, current_count -> f(0, days_til_spawn, 1) + current_count end)
    f(0, 3, 1)# + f(0, 4, 1) + f(0, 3, 1) + f(0, 1, 1) + f(0, 2, 1)
  end

  def parse_line(line),
    do: line |> String.trim() |> String.split(",") |> Enum.map(&String.to_integer/1)

  def advance_spawn_timers(spawn_timers) do
    spawn_timers |> Enum.flat_map(&advance_spawn_timer/1)
  end

  def advance_spawn_timer(0), do: [6, 8]
  def advance_spawn_timer(spawn_timer), do: [spawn_timer - 1]

  @number_days 256

  # Here d is the current day, the spawn timer, and c is the accumulated count.
  # The idea is that we do a sort of recursive depth first walk of a tree that relates a fish to the
  # the fish that spawned it. When we recur with a spawn timer set to 8, that's essentially spawning
  # a new fish. When we recur with a spawn timer set to 6, we are essentially advancing the current
  # fish (represented by the current invocation) to its next spawning time.
  #
  # This worked for the original part A example where there were 5 initial fish and checking after
  # 18 days. However, this recursive solution did not scale (took too long/never terminated) when
  # set to the full 256 days.
  def f(d, t, c) when d + t >= @number_days, do: c
  def f(d, t, c), do: f(d + t + 1, 6, f(d + t + 1, 8, c + 1))
end

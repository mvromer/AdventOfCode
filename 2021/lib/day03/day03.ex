defmodule Aoc2021.Day03 do
  use Aoc2021

  def solve_a do
    # Parses the line containing the next bitword into a list of bits.
    expand_bits = fn line ->
      line |> String.trim() |> String.graphemes() |> Enum.map(&String.to_integer/1)
    end

    # Does a bitwise sum between the current bitwise sums and the next bitword and increments the
    # number of bitwords that have been bitwise summed.
    bitwise_sum = fn next_bitword, {current_bitwise_sums, current_number_bitwords} ->
      {
        # Essentially we match (zip) bits in corresponding positions and sum them together to form
        # a new expanded bitword.
        Enum.zip_with(next_bitword, current_bitwise_sums, fn x, y -> x + y end),
        current_number_bitwords + 1
      }
    end

    # Computes the gamma and epsilon rates from the computed bitwise sum of the input bitwords.
    compute_rates = fn {bitwise_sums, number_bitwords} ->
      cutoff = div(number_bitwords, 2)

      # If the number of bits in a given position is greater than or equal to half the number of
      # bitwords processed, then the corresponding bit in the gamma rate is a 1; otherwise it's a 0.
      # The epsilon rate is the inverse of this.
      bitwise_sums
      |> Stream.map(fn total_ones -> if total_ones >= cutoff, do: [1, 0], else: [0, 1] end)
      |> Stream.zip_with(&Enum.join/1)
      |> Enum.map(&String.to_integer(&1, 2))
    end

    expanded_bitwords = @puzzle_input |> File.stream!() |> Stream.map(expand_bits)

    [gamma_rate, epsilon_rate] =
      expanded_bitwords
      |> Stream.drop(1)
      |> Enum.reduce({expanded_bitwords |> Enum.take(1) |> List.first(), 1}, bitwise_sum)
      |> compute_rates.()

    gamma_rate * epsilon_rate
  end

  def solve_b do
  end
end

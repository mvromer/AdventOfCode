defmodule Aoc2021.Day03 do
  use Aoc2021

  def solve_a do
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
      cutoff = number_bitwords / 2

      # If the number of bits in a given position is greater than or equal to half the number of
      # bitwords processed, then the corresponding bit in the gamma rate is a 1; otherwise it's a 0.
      # The epsilon rate is the inverse of this. In effect, what we're doing is building up each
      # rate one bit at a time and then joining the corresponding bits together and converting them
      # to a decimal value.
      bitwise_sums
      |> Stream.map(fn total_ones -> if total_ones >= cutoff, do: [1, 0], else: [0, 1] end)
      |> Stream.zip_with(&Enum.join/1)
      |> Enum.map(&String.to_integer(&1, 2))
    end

    expanded_bitwords = @puzzle_input |> File.stream!() |> Stream.map(&expand_bits/1)

    [gamma_rate, epsilon_rate] =
      expanded_bitwords
      |> Stream.drop(1)
      |> Enum.reduce({expanded_bitwords |> Enum.take(1) |> List.first(), 1}, bitwise_sum)
      |> compute_rates.()

    gamma_rate * epsilon_rate
  end

  def solve_b do
    select_o2_filter_bit = fn {bitsum, number_bitwords} ->
      if bitsum >= number_bitwords / 2, do: 1, else: 0
    end

    select_co2_filter_bit = fn {bitsum, number_bitwords} ->
      if bitsum < number_bitwords / 2, do: 1, else: 0
    end

    expanded_bitwords = @puzzle_input |> File.stream!() |> Enum.map(&expand_bits/1)
    o2_generator_rating = life_support_reducer(expanded_bitwords, [], select_o2_filter_bit)
    co2_scrubber_rating = life_support_reducer(expanded_bitwords, [], select_co2_filter_bit)

    o2_generator_rating * co2_scrubber_rating
  end

  defp life_support_reducer([first_bitword | []], rating_bits, _bit_selector) do
    rating_bits
    |> Enum.reduce(first_bitword, fn bit, bitword -> [bit | bitword] end)
    |> Enum.join()
    |> String.to_integer(2)
  end

  defp life_support_reducer(bitwords, rating_bits, bit_selector) do
    filter_bit =
      bitwords
      |> Stream.map(fn [bit | _] -> bit end)
      |> Enum.reduce({0, 0}, fn current_bit, {current_bitsum, current_number_bitwords} ->
        {current_bitsum + current_bit, current_number_bitwords + 1}
      end)
      |> bit_selector.()

    bitwords
    |> Stream.filter(fn [bit | _] -> bit === filter_bit end)
    |> Enum.map(fn [_ | rest] -> rest end)
    |> life_support_reducer([filter_bit | rating_bits], bit_selector)
  end

  # Parses the line containing the next bitword into a list of bits.
  defp expand_bits(line),
    do: line |> String.trim() |> String.graphemes() |> Enum.map(&String.to_integer/1)
end

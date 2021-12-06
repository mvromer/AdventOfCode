defmodule BingoCard do
  defstruct open_positions: %{}, called_row_counts: %{}, called_column_counts: %{}

  def new(card_lines) do
    open_positions =
      card_lines
      |> Stream.with_index()
      |> Stream.flat_map(fn {line, row} ->
        line
        |> Stream.with_index()
        |> Stream.map(fn {card_number, column} -> {card_number, {row, column}} end)
      end)
      |> Enum.into(%{})

    %BingoCard{
      open_positions: open_positions,
      called_row_counts: for(row <- 0..4, into: %{}, do: {row, 0}),
      called_column_counts: for(column <- 0..4, into: %{}, do: {column, 0})
    }
  end
end

defmodule Aoc2021.Day04 do
  use Aoc2021

  def solve_a do
    {called_numbers, bingo_cards} = read_bingo_game!()
    {final_number, first_winning_card} = find_first_winner!(called_numbers, bingo_cards)

    compute_final_score(final_number, first_winning_card)
  end

  def solve_b do
    {called_numbers, bingo_cards} = read_bingo_game!()
    {final_number, last_winning_card} = find_last_winner(called_numbers, bingo_cards)

    compute_final_score(final_number, last_winning_card)
  end

  def compute_final_score(final_number, bingo_card),
    do: final_number * (bingo_card.open_positions |> Map.keys() |> Enum.sum())

  defp read_bingo_game! do
    File.open!(@puzzle_input, [:read, :utf8], fn file ->
      called_nunmbers =
        case IO.read(file, :line) do
          line when is_binary(line) ->
            line
            |> String.trim()
            |> String.split(",")
            |> Enum.map(&String.to_integer/1)

          _ ->
            raise File.Error, message: "Failed to read input"
        end

      # Advance one line to the first bingo card.
      IO.read(file, :line)

      bingo_cards =
        Stream.unfold(file, fn file ->
          case read_bingo_card(file) do
            nil -> nil
            card -> {card, file}
          end
        end)
        |> Enum.to_list()

      {called_nunmbers, bingo_cards}
    end)
  end

  defp read_bingo_card(file) do
    number_card_lines = 5

    try do
      card_numbers =
        Stream.unfold(number_card_lines, fn
          0 ->
            nil

          remaining_card_lines ->
            case read_bingo_card_line(file) do
              line when is_list(line) -> {line, remaining_card_lines - 1}
              _ -> throw(:truncated_card)
            end
        end)
        |> Enum.to_list()
        |> BingoCard.new()

      # Advance past the blank line that follows this card.
      IO.read(file, :line)
      card_numbers
    catch
      :truncated_card -> nil
    end
  end

  defp read_bingo_card_line(file) do
    case IO.read(file, :line) do
      line when is_binary(line) ->
        line |> String.trim() |> String.split(" ", trim: true) |> Enum.map(&String.to_integer/1)

      _ ->
        nil
    end
  end

  defp find_first_winner!([], _), do: raise(RuntimeError, "No bingo winner found")

  defp find_first_winner!([called_number | remaining_numbers], bingo_cards) do
    case check_for_winner(called_number, bingo_cards) do
      winning_card when is_struct(winning_card) -> {called_number, winning_card}
      updated_cards -> find_first_winner!(remaining_numbers, updated_cards)
    end
  end

  defp check_for_winner(called_number, bingo_cards) do
    Enum.reduce_while(bingo_cards, [], fn bingo_card, updated_cards ->
      {updated_card, row, column} = mark_card(called_number, bingo_card)

      cond do
        updated_card.called_row_counts[row] == 5 -> {:halt, updated_card}
        updated_card.called_column_counts[column] == 5 -> {:halt, updated_card}
        true -> {:cont, [updated_card | updated_cards]}
      end
    end)
  end

  defp find_last_winner(called_numbers, bingo_cards) do
    called_numbers
    |> Enum.reduce({bingo_cards, []}, &update_card_partitions/2)
    |> elem(1)
    |> List.first()
  end

  defp update_card_partitions(called_number, {remaining_cards, current_winning_cards}) do
    %{remaining: new_remaining_cards, winning: new_winning_cards} =
      Map.merge(
        %{remaining: [], winning: []},
        partition_by_winning_state(called_number, remaining_cards)
      )

    {
      new_remaining_cards,
      Enum.reverse(new_winning_cards) ++ current_winning_cards
    }
  end

  defp partition_by_winning_state(called_number, bingo_cards) do
    bingo_cards
    |> Stream.map(fn bingo_card ->
      {updated_card, row, column} = mark_card(called_number, bingo_card)

      cond do
        updated_card.called_row_counts[row] == 5 ->
          {:winning, {called_number, updated_card}}

        updated_card.called_column_counts[column] == 5 ->
          {:winning, {called_number, updated_card}}

        true ->
          {:remaining, updated_card}
      end
    end)
    |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
  end

  defp mark_card(called_number, bingo_card) do
    case bingo_card.open_positions[called_number] do
      nil ->
        {bingo_card, nil, nil}

      {row, column} ->
        new_called_row_count = bingo_card.called_row_counts[row] + 1
        new_called_column_count = bingo_card.called_column_counts[column] + 1

        updated_card = %BingoCard{
          open_positions: Map.delete(bingo_card.open_positions, called_number),
          called_row_counts: %{
            bingo_card.called_row_counts
            | row => new_called_row_count
          },
          called_column_counts: %{
            bingo_card.called_column_counts
            | column => new_called_column_count
          }
        }

        {updated_card, row, column}
    end
  end
end

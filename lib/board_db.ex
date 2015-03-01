defmodule Snake.BoardDB do
  alias Snake.Board

  @table_name __MODULE__
  @table_options [:named_table,  # Use atom to adress the ETS table.
                  :private,      # Only owner can read/write from/to table.
                  {:keypos, 1},  # Key at position 1 of the table (1 - based)
                  :set]          # No duplicates allowed!

  # API:
  
  @doc """
  Initializes the table.
  """
  def init, do: :ets.new(@table_name, @table_options)

  @doc """
  Adds a board at position {x, y} to the table.
  """
  def add(key = {x, y}, board = {Board, _node}) when is_integer(x) 
                                                and is_integer(y) do
    # Pattern match fails if board already present at {X, Y}
    true = :ets.insert_new(@table_name, {key, board})
  end

  @doc """
  Searches for a board at position {x, y} from the table.
  Returns the node if a matching record is found, or :no_board if none found.
  """
  def get(key = {x, y}) when is_integer(x) and is_integer(y) do
    :ets.lookup(@table_name, key) |> handle_get_result
  end

  @doc """
  Deletes a board with position {x, y}.
  """
  def delete(key = {x, y}) when is_integer(x) and is_integer(y) do
    :ets.delete(@table_name, key)
  end

  @doc """
  Finds the corresponding position for a certain board.
  """
  def get_key(board = {Board, _node}) do
    :ets.match(@table_name, {:"$1", board}) |> handle_get_key_result 
  end

  # Helper functions:

  defp handle_get_result([]), do: :no_board
  defp handle_get_result([{_key, board}]), do: board

  defp handle_get_key_result([]), do: :no_position
  defp handle_get_key_result([[position]]), do: position
end

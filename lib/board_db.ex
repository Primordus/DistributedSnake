defmodule Snake.BoardDB do
  
  @table_name __MODULE__
  # Note: no named_table in options (would make tests harder to write)
  @table_options [:private,      # Only owner can read/write from/to table.
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
  def add(table, key = {x, y}, board) when is_integer(table)
                                      and is_integer(x) 
                                      and is_integer(y) do
    # Pattern match fails if board already present at {X, Y}
    true = :ets.insert_new(table, {key, board})
  end

  @doc """
  Searches for a board at position {x, y} from the table.
  Returns the node if a matching record is found, or :no_board if none found.
  """
  def get(table, key = {x, y}) when is_integer(table) 
                                and is_integer(x) 
                                and is_integer(y) do
    :ets.lookup(table, key) |> handle_get_result
  end

  @doc """
  Deletes a board with position {x, y}.
  """
  def delete(table, key = {x, y}) when is_integer(table) 
                                  and is_integer(x)
                                  and is_integer(y) do
    :ets.delete(table, key)
  end

  @doc """
  Finds the corresponding position for a certain board.
  """
  def get_key(table, board) when is_integer(table) do
    :ets.match(table, {:"$1", board}) |> handle_get_key_result 
  end

  # Helper functions:

  defp handle_get_result([]), do: :no_board
  defp handle_get_result([{_key, board}]), do: board

  defp handle_get_key_result([]), do: :no_position
  defp handle_get_key_result([[position]]), do: position
end

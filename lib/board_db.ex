defmodule Snake.BoardDB do
  
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
  def add(key = {_x, _y}, board = {:board, _node}) do
    # Pattern match fails if board already present at {X, Y}
    true = :ets.insert_new(@table_name, {key, board})
  end

  @doc """
  Searches for a board at position {x, y} from the table.
  Returns the node if a matching record is found, or :no_board if none found.
  """
  def get(key = {_x, _y}) do
    :ets.lookup(@table_name, key) |> handle_get_result
  end

  @doc """
  Finds the corresponding position for a certain board.
  """
  def get_key(board = {:board, _node}) do
    :ets.match(@table_name, {:"$1", board}) |> handle_get_key_result 
  end

  # Helper functions:

  defp handle_get_result([]), do: :no_board
  defp handle_get_result([{_key, board}]), do: board

  defp handle_get_key_result([]), do: :no_position
  defp handle_get_key_result([position]), do: position
end

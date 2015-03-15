defmodule Snake.TileDB do
  
  @table __MODULE__
  # Note: no named_table in options (would make tests harder to write)
  @table_options [:named_table,  # Access table using name.
                  :public,       # Everybody can read/write from/to table.
                  {:keypos, 1},  # Key at position 1 of the table (1 - based)
                  :set]          # No duplicates allowed!

  # API:

  @doc """
  Initializes the table.
  """
  def init, do: @table |> :ets.info |> do_init
  
  @doc """
  Adds a tile at position {x, y} to the table.
  """
  def add(key = {x, y}, tile) when is_integer(x) 
                                   and x > -1
                                   and is_integer(y)
                                   and y > -1
                                   and is_pid(tile) do
    true = @table |> :ets.insert_new {key, tile}
  end

  @doc """
  Searches for a tile at position {x, y} from the table.
  Returns the tile pid if a matching tile is found, or :no_tile if none found.
  """
  def get(key = {x, y}) when is_integer(x) and is_integer(y) do
    @table |> :ets.lookup(key) |> handle_result
  end

  @doc """
  Deletes a tile at position {x, y}
  """
  def delete(key = {x, y}) when is_integer(x) and is_integer(y) do
    @table |> :ets.delete key
  end

  # Helper functions
  defp do_init(:undefined), do: :ets.new(@table, @table_options)
  defp do_init(_), do: @table
  
  defp handle_result([]), do: :no_tile
  defp handle_result([{_key, tile}]), do: tile
end

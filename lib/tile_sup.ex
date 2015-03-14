defmodule Snake.TileSupervisor do
  use Supervisor
  use Snake.Game
  alias Snake.Tile
  alias Snake.TileDB

  @moduledoc """
  Supervisor for tile processes.
  """

  @sup __MODULE__

  # API

  @doc """
  Starts the supervisor, initializes the tile ETS table and 
  spawns tile processes for each {X, Y} in the game (on this node).
  """
  def start_link(:ok) do
    {:ok, pid} = Supervisor.start_link(__MODULE__, :ok, [name: @sup])
    TileDB.init |> spawn_tiles
    {:ok, pid}
  end

  @doc """
  Starts a tile process in the supervision tree.
  """
  def start_child(args = %{table: _table, x: _x, y: _y}) do
    @sup |> Supervisor.start_child [args]
  end

  # Supervisor callbacks

  @doc false
  def init(:ok) do
    tree = [worker(Tile, [])]
    supervise(tree, strategy: :simple_one_for_one)
  end

  # Helper functions

  defp spawn_tiles(:finished), do: :ok
  defp spawn_tiles(args = {{_x, _y}, _table}) do
    args 
    |> spawn_tile
    |> next_position
    |> spawn_tiles
  end
  defp spawn_tiles(table), do: spawn_tiles {{width, height}, table}

  defp spawn_tile(args = {{x, y}, table}) do
    start_child %{table: table, x: x, y: y}
    args
  end

  defp next_position({{-1, -1}, _table}), do: :finished
  defp next_position({{x, -1}, table}), do: {{x - 1, height}, table}
  defp next_position({{x, y}, table}), do: {{x, y - 1}, table}
end

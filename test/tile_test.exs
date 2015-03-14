defmodule Snake.TileTest do
  use ExUnit.Case, async: true
  use Snake.Game
  alias Snake.Tile
  alias Snake.TileDB
  alias Snake.TileSupervisor, as: TileSup

  setup_all do
    Application.ensure_all_started :snake
  end

  # Tile supervisor tests
  test "Tile and tile supervisor starts correctly" do
    refute Process.whereis(TileSup) == nil
    assert count_children(TileSup) == width * height
  end

  # Tile DB tests

  test "Creating tile table" do
    table = TileDB.init
    assert is_integer(table)
    refute :ets.info(table) == :undefined
  end

  test "CRUD operations" do
    e = FunctionClauseError
    pos = {0, 1000}
    table = TileDB.init

    # Create
    assert_raise e, fn -> table |> TileDB.add :bla, self end
    assert_raise e, fn -> table |> TileDB.add {:bla, 0}, self end
    assert_raise e, fn -> table |> TileDB.add {0, :bla}, self end 
    assert_raise e, fn -> table |> TileDB.add {0, 0}, :bla end 
    assert table |> TileDB.add(pos, self)

    # Read
    assert_raise e, fn -> table |> TileDB.delete :bla end
    assert_raise e, fn -> table |> TileDB.delete {:bla, 0} end
    assert_raise e, fn -> table |> TileDB.delete {0, :bla} end
    assert table |> TileDB.get({-1, -1}) == :no_tile
    assert table |> TileDB.get(pos) == self

    # Update => not supported

    # Delete
    table |> TileDB.delete(pos)
    assert table |> TileDB.get(pos) == :no_tile
  end

  # Tile tests

  test "Notify arrival / gone (snake)" do
    e = MatchError
    snake = self
    table = TileDB.init
    {:ok, tile} = Tile.start_link %{table: table, x: 5, y: 5}
    
    assert :ok == Tile.notify_arrival(tile, :snake, snake, :red)
    assert_raise e, fn -> Tile.notify_arrival(tile, :snake, snake, :green) end
    
    assert :ok == tile |> Tile.notify_gone(snake)
    assert_raise e, fn -> Tile.notify_gone(tile, snake) end
  end

  test "Notify arrival / gone (insect)" do
    e = MatchError
    insect = self
    table = TileDB.init
    {:ok, tile} = Tile.start_link %{table: table, x: 5, y: 5}
    
    assert :ok == Tile.notify_arrival(tile, :insect, insect)
    assert_raise e, fn -> Tile.notify_arrival(tile, :insect, insect) end

    assert :ok == tile |> Tile.notify_gone(insect)
    assert_raise e, fn -> Tile.notify_gone(tile, insect) end
  end

  test "Notify arrival / gone (snake + insect)" do
    # First snake, then insect
    e = MatchError
    snake = self
    insect = spawn_dummy
    table = TileDB.init
    {:ok, tile} = Tile.start_link %{table: table, x: 5, y: 5}
    
    assert :ok == Tile.notify_arrival(tile, :snake, snake, :red)
    assert_raise e, fn -> Tile.notify_arrival(tile, :insect, insect) end
    
    assert :ok == Tile.notify_gone(tile, snake)

    # First insect, then snake
    assert :ok == Tile.notify_arrival(tile, :insect, insect)
    assert_raise e, fn -> Tile.notify_arrival(tile, :snake, snake, :green) end
  end

  # TODO test guards!

  # Helper functions

  defp count_children(sup) do
    sup 
    |> Supervisor.which_children 
    |> length
  end

  defp spawn_dummy do
    spawn_link fn ->
      receive do
        _ -> :ok
      end
    end
  end
end

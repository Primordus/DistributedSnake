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

  test "Tile table is created in supervision tree, can only be created once" do
    info = :ets.info(TileDB)
    refute info == :undefined
    TileDB.init
    assert :ets.info(TileDB) == info
  end

  test "CRUD operations" do
    e = FunctionClauseError
    pos = {0, 1000}

    # Create
    assert_raise e, fn -> TileDB.add :bla, self end
    assert_raise e, fn -> TileDB.add {:bla, 0}, self end
    assert_raise e, fn -> TileDB.add {0, :bla}, self end 
    assert_raise e, fn -> TileDB.add {0, 0}, :bla end 
    assert TileDB.add(pos, self)

    # Read
    assert_raise e, fn -> TileDB.delete :bla end
    assert_raise e, fn -> TileDB.delete {:bla, 0} end
    assert_raise e, fn -> TileDB.delete {0, :bla} end
    assert TileDB.get({-1, -1}) == :no_tile
    assert TileDB.get(pos) == self

    # Update => not supported

    # Delete
    TileDB.delete(pos)
    assert TileDB.get(pos) == :no_tile
  end

  # Tile tests

  test "Notify arrival / gone (snake)" do
    e = MatchError
    snake = self
    {:ok, tile} = spawn_tile %{x: 5, y: 5}
    
    assert :ok == Tile.notify_arrival(tile, :snake, snake, :red)
    assert_raise e, fn -> Tile.notify_arrival(tile, :snake, snake, :green) end
    
    assert :ok == tile |> Tile.notify_gone(snake)
    assert_raise e, fn -> Tile.notify_gone(tile, snake) end
  end

  test "Notify arrival / gone (insect)" do
    e = MatchError
    insect = self
    {:ok, tile} = spawn_tile %{x: 6, y: 6}
    
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
    {:ok, tile} = spawn_tile %{x: 7, y: 7}
    
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

  defp spawn_tile(args = %{x: x, y: y}) do
    # First delete from table (supervision tree starts 100 tiles already)
    # Otherwise a double entry would be added to table => forbidden!
    TileDB.delete {x, y}
    Tile.start_link args
  end
end

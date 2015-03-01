defmodule Snake.BoardDBTest do
  use ExUnit.Case, async: true
  alias Snake.BoardDB
  alias Snake.Board

  @moduledoc """
  Tests for the board database.
  """

  test "Creating table" do
      assert :ets.info(BoardDB) == :undefined
      BoardDB.init
      refute :ets.info(BoardDB) == :undefined
  end

  test "CRUD operations" do
    error = FunctionClauseError
    BoardDB.init
    
    # Create
    assert_raise error, fn -> BoardDB.add {0, 0}, :bla end
    assert_raise error, fn -> BoardDB.add :bla, {Board, :dummy_node} end
    assert_raise error, fn -> BoardDB.add {:bla, 0}, {Board, :dummy_node} end
    assert_raise error, fn -> BoardDB.add {0, :bla}, {Board, :dummy_node} end
    assert BoardDB.add {0, 1}, {Board, :dummy_node}
    assert_raise MatchError, fn -> BoardDB.add {0, 1}, {Board, :dummy_node} end

    # Read
    assert_raise error, fn -> BoardDB.get :bla end
    assert_raise error, fn -> BoardDB.get {:bla, 0} end
    assert_raise error, fn -> BoardDB.get {0, :bla} end
    assert :no_board == BoardDB.get {0, 0}
    assert {Board, :dummy_node} == BoardDB.get {0, 1}

    # Update => not supported.

    # Delete
    assert_raise error, fn -> BoardDB.delete :bla end
    assert_raise error, fn -> BoardDB.delete {:bla, 0} end
    assert_raise error, fn -> BoardDB.delete {0, :bla} end
    BoardDB.delete {0, 1}
    assert :no_board == BoardDB.get {0, 1}
  end

  test "Search key {x, y} based on value (board)" do
    search_value1 = {Board, :dummy_node1}
    search_value2 = {Board, :dummy_node2}
    search_value3 = {Board, :dummy_node3}

    position1 = {0, 0}
    position2 = {0, 1}

    BoardDB.init
    BoardDB.add position1, search_value1
    BoardDB.add position2, search_value2
    assert position1 == BoardDB.get_key search_value1
    assert position2 == BoardDB.get_key search_value2
    assert :no_position == BoardDB.get_key search_value3
  end
end

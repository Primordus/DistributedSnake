defmodule Snake.BoardTest do
  use ExUnit.Case
  alias Snake.Gossip
  alias Snake.Board
  alias Snake.BoardManager
  alias Snake.BoardManager.State
  alias Snake.BoardDB

  @moduledoc """
  Tests for Board, BoardManager (including state) and BoardDB.
  """

  setup_all do
    # Check if board = local registered process:
    assert Process.whereis(Board) == nil
    Board.start_link
    refute Process.whereis(Board) == nil
    {:error, {:already_started, _}} = Board.start_link
    assert :ok == Board.stop
    sleep 2
    assert Process.whereis(Board) == nil

    # Check if board manager = global registered process:
    refute BoardManager in :global.registered_names
    Gossip.start_link
    BoardManager.start_link
    assert BoardManager in :global.registered_names


    # Initialize 2 boards (1 registered, 1 unregistered)
    board1 = new_board
    {:ok, board2} = Board.start_link

    {:ok, board1: board1, board2: board2}
  end

  # Tests for board module:

  test "Get all boards" do
    expected_result = Enum.map [Node.self | Node.list], fn(node) ->
      {Board, node}
    end
    assert Board.get_all == expected_result
  end

  test "Get local board", do: assert Board.local_board == {Board, Node.self}

  test "Adding and removing board (up)", %{board1: board1, board2: board2} do
    assert Board.get(:up) == :no_board

    assert :ok == Board.add(:no_board, :up_of, board2)
    assert :ok == Board.add(board2, :up_of, :no_board)

    Board.add board1, :up_of, board2
    assert Board.get(:up) == board1

    # Adding no board should have no effect.
    assert :ok == Board.add(:no_board, :up_of, board2)
    assert :ok == Board.add(board2, :up_of, :no_board)
    assert Board.get(:up) == board1

    # Removing :no_board should have no effect.
    assert :ok == Board.remove(:up_of, :no_board)
    assert Board.get(:up) == board1

    Board.remove :up_of, board2
    assert Board.get(:up) == :no_board
  end

  test "Adding and removing board (down)", %{board1: board1, board2: board2} do
    assert Board.get(:down) == :no_board

    assert :ok == Board.add(:no_board, :down_of, board2)
    assert :ok == Board.add(board2, :down_of, :no_board)

    Board.add board1, :down_of, board2
    assert Board.get(:down) == board1

    # Adding no board should have no effect.
    assert :ok == Board.add(:no_board, :down_of, board2)
    assert :ok == Board.add(board2, :down_of, :no_board)
    assert Board.get(:down) == board1

    # Removing :no_board should have no effect.
    assert :ok == Board.remove(:down_of, :no_board)
    assert Board.get(:down) == board1

    Board.remove :down_of, board2
    assert Board.get(:down) == :no_board
  end

  test "Adding and removing right board (right)", %{board1: board1, board2: board2} do
    assert Board.get(:right) == :no_board
    
    assert :ok == Board.add(:no_board, :right_of, board2)
    assert :ok == Board.add(board2, :right_of, :no_board)

    Board.add board1, :right_of, board2
    assert Board.get(:right) == board1

    # Adding no board should have no effect.
    assert :ok == Board.add(:no_board, :right_of, board2)
    assert :ok == Board.add(board2, :right_of, :no_board)
    assert Board.get(:right) == board1

    # Removing :no_board should have no effect.
    assert :ok == Board.remove(:right_of, :no_board)
    assert Board.get(:right) == board1

    Board.remove :right_of, board2
    assert Board.get(:right) == :no_board
  end

  test "Adding and removing board (left)", %{board1: board1, board2: board2} do
    assert Board.get(:left) == :no_board

    assert :ok == Board.add(:no_board, :left_of, board2)
    assert :ok == Board.add(board2, :left_of, :no_board)

    Board.add board1, :left_of, board2
    assert Board.get(:left) == board1

    # Adding no board should have no effect.
    assert :ok == Board.add(:no_board, :left_of, board2)
    assert :ok == Board.add(board2, :left_of, :no_board)
    assert Board.get(:left) == board1

    # Removing :no_board should have no effect.
    assert :ok == Board.remove(:left_of, :no_board)
    assert Board.get(:left) == board1

    Board.remove :left_of, board2
    assert Board.get(:left) == :no_board
  end 

  # Board DB tests:

  test "Creating table" do
      table = BoardDB.init
      refute :ets.info(table) == :undefined
      assert is_integer(table)
  end

  test "CRUD operations" do
    error = FunctionClauseError
    table = BoardDB.init
    
    # Create
    assert_raise error, fn -> 
      table |> BoardDB.add :bla, {Board, :dummy_node} end
    assert_raise error, fn -> 
      table |> BoardDB.add {:bla, 0}, {Board, :dummy_node} 
    end
    assert_raise error, fn -> 
      table |> BoardDB.add {0, :bla}, {Board, :dummy_node} 
    end
    assert table |> BoardDB.add {0, 1}, {Board, :dummy_node}
    assert_raise MatchError, fn -> 
      table |> BoardDB.add {0, 1}, {Board, :dummy_node} 
    end

    # Read
    assert_raise error, fn -> table |> BoardDB.get :bla end
    assert_raise error, fn -> table |> BoardDB.get {:bla, 0} end
    assert_raise error, fn -> table |> BoardDB.get {0, :bla} end
    assert :no_board == table |> BoardDB.get {0, 0}
    assert {Board, :dummy_node} == table |> BoardDB.get {0, 1}

    # Update => not supported.

    # Delete
    assert_raise error, fn -> table |> BoardDB.delete :bla end
    assert_raise error, fn -> table |> BoardDB.delete {:bla, 0} end
    assert_raise error, fn -> table |> BoardDB.delete {0, :bla} end
    table |> BoardDB.delete {0, 1}
    assert :no_board == table |> BoardDB.get {0, 1}
  end

  test "Search key {x, y} based on value (board)" do
    search_value1 = {Board, :dummy_node1}
    search_value2 = {Board, :dummy_node2}
    search_value3 = {Board, :dummy_node3}

    position1 = {0, 0}
    position2 = {0, 1}

    table = BoardDB.init
    table |> BoardDB.add position1, search_value1
    table |> BoardDB.add position2, search_value2
    assert position1 == table |> BoardDB.get_key search_value1
    assert position2 == table |> BoardDB.get_key search_value2
    assert :no_position == table |> BoardDB.get_key search_value3
  end

  # Board manager state tests:

  test "Add gaps to state" do
    position0 = {0, 1}
    position1 = {3, 1}
    position2 = :no_position
    position3 = :bla
    
    state0 = %State{}
    state1 = state0 |> State.add_gap position0
    state2 = state1 |> State.add_gap position1
    state3 = state2 |> State.add_gap position2
    
    assert_raise FunctionClauseError, fn -> 
      state3 |> State.add_gap position3
    end
    
    assert state0.gaps == []
    assert state1.gaps == [position0]
    assert state2.gaps == [position0, position1]
    assert state2 == state3
  end
  
  test "State: Spiral movement" do
    first_gap = {1, 0}
    second_gap = {1, 1}

    state0 = %State{}
    state1 = state0 |> State.next_state
    state2 = state1 |> State.next_state
    state3 = state2 |> State.next_state
    state4 = state3 |> State.next_state
    state5 = state4 |> State.next_state
    state6 = state5 |> State.next_state
    state7 = state6 |> State.next_state
    state8 = state7 |> State.add_gap first_gap
    state9 = state8 |> State.add_gap second_gap
    state10 = state9 |> State.next_state
    state11 = state10 |> State.next_state
    state12 = state11 |> State.next_state
    assert state0.position == {0, 0}
    assert state1.position == {1, 0}
    assert state2.position == {1, -1}
    assert state3.position == {0, -1}
    assert state4.position == {-1, -1}
    assert state5.position == {-1, 0}
    assert state6.position == {-1, 1}
    assert state7.position == {0, 1}
    assert state8.position == {0, 1}  # just adds gap to the state.
    assert state9.position == {0, 1}  # just adds gap to the state.
    assert state10.position == first_gap
    assert state11.position == second_gap
    assert state12.position == {0, 1}
  end

  # Board manager tests:

  test "Adding and removing of board", context do
    unregistered_board = context[:board1]
    registered_board = context[:board2]

    # Create setup: registered_board right of unregistered_board, 
    # unregistered_board down of registered_board
    assert BoardManager.notify_board_added(unregistered_board) == :ok
    assert BoardManager.notify_board_added(registered_board) == :ok
    assert BoardManager.notify_board_added(unregistered_board) == :ok
    sleep 20
    
    assert Board.get(:left_of, registered_board) == unregistered_board
    assert Board.get(:down_of, registered_board) == unregistered_board
    assert Board.get(:up_of, unregistered_board) == registered_board
    assert Board.get(:right_of, unregistered_board) == registered_board

    assert BoardManager.notify_board_gone(registered_board) == :ok
    
    # Manually remove links from registered_board (otherwise other tests fail).
    # This is OK because in normal situation the notification is only sent from
    # a board that is about to stop.
    Board.remove(:left_of, registered_board)
    Board.remove(:down_of, registered_board)

    sleep 10
    assert Board.get(:right_of, unregistered_board) == :no_board
    assert Board.get(:up_of, unregistered_board) == :no_board
  end

  # Helper functions

  defp new_board do
    {:ok, board} = Board.start_link
    Process.unregister Board
    board
  end

  def sleep(ms), do: :timer.sleep(ms)
end

defmodule Snake.BoardTest do
  use ExUnit.Case, async: true
  alias Snake.Board
  alias Snake.BoardManager

  setup_all do
    test_registered_process # TODO refactor this test later!

    # Prevent weird errors on termination by spawning a dummy manager
    spawn_dummy_board_manager
    
    # Initialize 2 boards
    {:ok, board1} = spawn_unregistered_board
    {:ok, board2} = Board.start_link

    {:ok, board1: board1, board2: board2}
  end

  test "Get all boards" do
    expected_result = Enum.map [Node.self | Node.list], fn(node) ->
      {Board, node}
    end
    assert Board.get_all == expected_result
  end

  test "Get local board", do: assert Board.local_board == {Board, Node.self}

  # TODO also test if board on other side has :no_board everywhere?
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

  defp test_registered_process do
    assert Process.whereis(Board) == nil
    Board.start_link
    refute Process.whereis(Board) == nil
    {:error, {:already_started, _}} = Board.start_link
    assert :ok == Board.stop
    assert Process.whereis(Board) == nil
  end

  defp spawn_dummy_board_manager do
    dummy_mgr = spawn_link fn -> dummy_manager_loop end
    :global.register_name BoardManager, dummy_mgr
  end

  defp dummy_manager_loop do
    # Throw away all messages. (only message to board manager is a cast
    # so no response needed..)
    receive do
      _ -> :ok
    end

    dummy_manager_loop
  end

  defp spawn_unregistered_board do
    {:ok, board_pid} = Board.start_link
    Process.unregister Board
    {:ok, board_pid}
  end
end

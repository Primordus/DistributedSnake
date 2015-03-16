defmodule Snake.SnakeTest do
  use ExUnit.Case
  use Snake.Game
  alias Snake.SnakeSupervisor
  alias Snake.Snake.State
  alias Snake.Snake

  @moduledoc """
  Tests for application and supervisor tree.
  """

  setup_all do
    Application.ensure_all_started :snake
    sleep 30
  end

  # Tests regarding snake state.

  test "Creating new state" do
    size = 3
    state = State.new(size)
    
    assert state.size == size
    assert length(state.positions) == size
    assert state.direction in [:left, :right, :up, :down]
    assert state.color in [:orange, :green, :blue, :cyan, 
                           :yellow, :purple, :white]

    state.positions |> Enum.map fn({x, y, a_node}) ->
      assert x in 0..width
      assert y in 0..height
      assert a_node in [Node.self | Node.list]
    end
  end

  test "moving to the right in a straight line" do
    state = %State{State.new(3) 
                  | direction: :right,
                    positions: [{3, 2, node}, {2, 2, node}, {1, 2, node}]}
    new_state = State.move(state)

    Enum.zip(state.positions, new_state.positions) 
    |> Enum.map fn({{x1, y1, n1}, {x2, y2, n2}}) ->
      assert x2 == x1 + 1
      assert y2 == y1
      assert n2 == n1
    end
  end
  
  test "moving to the left in a straight line" do
    state = %State{State.new(3) 
                  | direction: :left,
                    positions: [{5, 5, node}, {6, 5, node}, {7, 5, node}]}
    new_state = State.move(state)

    Enum.zip(state.positions, new_state.positions) 
    |> Enum.map fn({{x1, y1, n1}, {x2, y2, n2}}) ->
      assert x2 == x1 - 1
      assert y2 == y1
      assert n2 == n1
    end
  end
  
  test "moving up in a straight line" do
    state = %State{State.new(3) 
                  | direction: :up,
                    positions: [{4, 4, node}, {4, 3, node}, {4, 2, node}]}
    new_state = State.move(state)

    Enum.zip(state.positions, new_state.positions) 
    |> Enum.map fn({{x1, y1, n1}, {x2, y2, n2}}) ->
      assert x2 == x1
      assert y2 == y1 + 1
      assert n2 == n1
    end
  end
  
  test "moving down in a straight line" do
    state = %State{State.new(3) 
                  | direction: :down,
                    positions: [{8, 7, node}, {8, 8, node}, {8, 9, node}]}
    new_state = State.move(state)

    Enum.zip(state.positions, new_state.positions) 
    |> Enum.map fn({{x1, y1, n1}, {x2, y2, n2}}) ->
      assert x2 == x1
      assert y2 == y1 - 1
      assert n2 == n1
    end
  end

  test "moving around corners (left)" do
    state = %State{State.new(2) 
                  | direction: :left,
                    positions: [{5, 5, node}, {5, 6, node}]}
    new_state = State.move(state)

    [{x1, y1, n1}, {x2, y2, n2}] = new_state.positions
    assert x1 == 4
    assert y1 == 5
    assert n1 == node

    assert x2 == 5
    assert y2 == 5
    assert n2 == node
  end

  test "moving around corners (right)" do
    state = %State{State.new(2) 
                  | direction: :right,
                    positions: [{5, 5, node}, {5, 6, node}]}
    new_state = State.move(state)

    [{x1, y1, n1}, {x2, y2, n2}] = new_state.positions
    assert x1 == 6
    assert y1 == 5
    assert n1 == node
    
    assert x2 == 5
    assert y2 == 5
    assert n2 == node
  end 
  
  test "moving around corners (up)" do
    state = %State{State.new(2) 
                  | direction: :up,
                    positions: [{5, 5, node}, {4, 5, node}]}
    new_state = State.move(state)

    [{x1, y1, n1}, {x2, y2, n2}] = new_state.positions
    assert x1 == 5
    assert y1 == 6
    assert n1 == node

    assert x2 == 5
    assert y2 == 5
    assert n2 == node
  end 
  
  test "moving around corners (down)" do
    state = %State{State.new(2) 
                  | direction: :down,
                    positions: [{5, 5, node}, {4, 5, node}]}
    new_state = State.move(state)

    [{x1, y1, n1}, {x2, y2, n2}] = new_state.positions
    assert x1 == 5
    assert y1 == 4
    assert n1 == node

    assert x2 == 5
    assert y2 == 5
    assert n2 == node
  end

  test "Adding segments to the snake" do
    size = 3
    state = State.new(size)
    new_state = State.add_segment(state)

    old_pos = state.positions
    new_pos = new_state.positions
    [pos1, pos2 | _tail] = Enum.reverse(new_pos)

    assert new_state.size == size + 1
    assert Enum.reverse(old_pos) == tl(Enum.reverse(new_pos))
    assert pos1 == pos2  # Last segment on same location as last.
  end

  # Tests regarding snake process
  
  test "Snake can give back the current location it occupies" do
    Snake.get_positions 
    |> Enum.map fn({x, y, n}) ->
      assert is_integer(x)
      assert is_integer(y)
      assert x in 0..width
      assert y in 0..height
      assert n in [Node.self | Node.list]
    end
  end

  test "SnakeSupervisor is a locally registered process" do
    refute Process.whereis(SnakeSupervisor) == nil
  end

  test "Snake is a globally registered process" do
    assert Snake in :global.registered_names
  end

  test "Snake reacts to ticks from Ticker process" do
    # Also tests update_state pipeline!
    positions1 = Snake.get_positions
    simulate_tick
    positions2 = Snake.get_positions
    refute positions1 == positions2
  end

  # TODO test snake collisions, insect collisions later.

  # Helper functions

  defp snake, do: {:global, Snake}

  defp simulate_tick do
    :ok = snake |> GenServer.call :update_state
  end

  defp sleep(ms), do: :timer.sleep(ms)
end

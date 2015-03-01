defmodule Snake.BoardManager do
  use GenServer
  alias Snake.Gossip
  alias Snake.Board
  alias Snake.BoardDB

  # TODO ASCII art for better explanation of movement
  # TODO use :gen_fsm?

  @moduledoc """
  This module contains code for a process that can dynamically connect and
  remove board processes from each other.
  """

  @server __MODULE__

  defmodule State do

    @moduledoc """
    Module for the state of the board manager, has a few helper functions
    to update the state.
    """

    defstruct position: {0, 0}, 
              last_direction: :right, 
              steps_left: 2,
              step_counter: 1, # memorizes how many steps per side of the spiral
              gaps: [], 
              old_position_saved: false

    @doc """
    Makes a spiral movement for adding boards, with priority to filling gaps.
    """
    def next_state(state = %State{position: old_position,
                                  last_direction: old_direction,
                                  steps_left: 0,
                                  step_counter: steps,
                                  gaps: []}) do
      # TODO test entire function with unit test later!
      # Special case: change direction + update steps and step counter.
      new_direction = old_direction |> change_direction
      new_position = {old_position, new_direction} |> change_position
      %State{state | position: new_position,
                      last_direction: new_direction,
                      step_counter: steps + 1,
                      steps_left: 2 * (steps + 1),
                      old_position_saved: false}
    end
    def next_state(state = %State{position: old_position,
                                  last_direction: old_direction,
                                  steps_left: steps,
                                  step_counter: steps,
                                  gaps: []}) do
      # Special case: first direction change
      # (+ update steps and step counter)
      new_direction = old_direction |> change_direction
      new_position = {old_position, new_direction} |> change_position
      %State{state | position: new_position,
                      last_direction: new_direction,
                      steps_left: steps - 1,
                      old_position_saved: false}
    end
    def next_state(state = %State{position: old_position,
                                  last_direction: direction,
                                  steps_left: steps,
                                  gaps: []}) do
      # Normal case: move in same direction and update steps
      new_position = {old_position, direction} |> change_position
      %State{state | position: new_position,
                      steps_left: steps - 1,
                      old_position_saved: false}
    end
    def next_state(state = %State{position: old_position,
                                  gaps: [first | rest],
                                  old_position_saved: false}) do
      # Special case: gaps => fill up oldest one first!
      # TODO change to closest gap to middle + distance measurement?
      new_gaps = rest |> append_to_list(old_position)
      %State{state | position: first,
                      gaps: new_gaps,
                      old_position_saved: true}
    end
    def next_state(state = %State{gaps: [first | rest],
                                  old_position_saved: true}) do
      # Special case: gaps => fill up oldest one first! 
      # TODO change to closest gap to middle + distance measurement?
      %State{state | position: first, gaps: rest}
    end

    @doc """
    Adds a gap to the list of gaps that need to be filled again.
    """
    def add_gap(state = %State{}, :no_position), do: state
    def add_gap(state = %State{gaps: gaps}, position = {_x, _y}) do
      %State{gaps: gaps |> append_to_list(position)}
    end

    defp change_direction(:right), do: :down
    defp change_direction(:down), do: :left
    defp change_direction(:left), do: :up
    defp change_direction(:up), do: :right

    defp change_position({{x, y}, :right}), do: {x + 1, y}
    defp change_position({{x, y}, :down}), do: {x, y - 1}
    defp change_position({{x, y}, :up}), do: {x, y + 1}
    defp change_position({{x, y}, :left}), do: {x - 1, y}

    defp append_to_list(list, item) do
      list |> Enum.reverse |> Enum.reverse(item)
    end
  end

  # API:
  
  @doc """
  Starts a board manager process.
  """
  def start_link do
    args = :ok
    GenServer.start_link(__MODULE__, args, [name: {:global, @server}])
  end

  @doc """
  Notifies the board manager that a board is gone.
  """
  def notify_board_gone(node) do
    @server |> GenServer.call {:removed_node, node}
  end

  # GenServer callbacks:

  @doc false
  def init(:ok) do
    Process.flag(:trap_exit, true)
    BoardDB.init
    Gossip.subscribe fn
      (msg = {:added_node, node}) ->
        :ok = GenServer.call(@server, msg)
      ({:removed_node, node}) ->
        :ok = notify_board_gone(node)
    end

    {:ok, %State{}}
  end

  @doc false
  def handle_call({:added_node, node}, _from, 
                  state = %State{position: position}) do
    add_board(position, node)
    {:reply, :ok, state |> State.next_state}
  end
  def handle_call({:removed_node, node}, _from, state = %State{}) do
    {:removed, position} = remove_board(node)
    {:reply, :ok, state |> State.add_gap(position)}
  end
  def handle_call(_request ,_from, state = %State{}) do
    {:reply, {:error, :not_supported}, state}
  end
  
  @doc false
  def terminate(_reason, state = %State{}) do
    Gossip.unsubscribe
    BoardDB.destroy
    :ok
  end

  # Helper functions:

  defp add_board(position = {x, y}, node) do
    new_board = node |> to_board
    BoardDB.add(position, new_board)
    
    up = BoardDB.get {x, y + 1} 
    down = BoardDB.get {x, y - 1}
    right = BoardDB.get {x + 1, y}
    left = BoardDB.get {x - 1, y}

    Board.add(up, :up_of, new_board)
    Board.add(down, :down_of, new_board)
    Board.add(right, :right_of, new_board)
    Board.add(left, :left_of, new_board)

    :ok
  end

  defp remove_board(node) do
    # Note: the node is already removed from the cluster!
    # This makes it impossible to simply clean up from the board process itself.
    node 
    |> to_board 
    |> BoardDB.get_key
    |> do_remove(node)
  end

  defp do_remove(:no_position, node) do
    {:removed, :no_position}
  end
  defp do_remove(position = {x, y}, node) do
    BoardDB.remove(position)

    up = BoardDB.get {x, y + 1} 
    down = BoardDB.get {x, y - 1}
    right = BoardDB.get {x + 1, y}
    left = BoardDB.get {x - 1, y}

    Board.remove(:down_of, up)
    Board.remove(:up_of, down)
    Board.remove(:left_of, right)
    Board.remove(:right_of, left)

    {:removed, position}
  end

  defp to_board(node), do: {:board, node}
end

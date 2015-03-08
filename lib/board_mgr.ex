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

  @server {:global, __MODULE__}

  defmodule State do

    @moduledoc """
    Module for the state of the board manager, has a few helper functions
    to update the state.
    """

    defstruct table: :no_pid,
              position: {0, 0}, 
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
      # Special case: change direction + update steps and step counter.
      new_direction = old_direction |> change_direction
      new_position = {old_position, new_direction} |> change_position
      %State{state | position: new_position,
                      last_direction: new_direction,
                      step_counter: steps + 1,
                      steps_left: 2 * steps + 1, # TODO error here?
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
      %State{state | gaps: gaps |> append_to_list(position)}
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
      list |> Enum.reverse |> Enum.reverse([item])
    end
  end



  # API:
  
  @doc """
  Starts a board manager process.
  """
  def start_link do
    GenServer.start_link(__MODULE__, args, [name: @server])
  end

  @doc """
  Notifies the board manager that a board has been added.
  """
  def notify_board_added(board) do
    @server |> GenServer.cast {:board_added, board} # TODO make this a call?
  end

  @doc """
  Notifies the board manager that a board is gone.
  """
  def notify_board_gone(board) do
    @server |> GenServer.cast {:board_gone, board} # TODO make this a call again?
  end

  # GenServer callbacks:

  @doc false
  def init(:ok) do
    Process.flag(:trap_exit, true)
    table = BoardDB.init
    Gossip.subscribe self, fn
      ({:added_node, node}) ->
        node 
        |> to_board 
        |> notify_board_added
      ({:removed_node, node}) ->
        node 
        |> to_board 
        |> notify_board_gone
    end

    {:ok, %State{table: table}}
  end

  @doc false
  def handle_call(_request ,_from, state = %State{}) do
    {:reply, {:error, :not_supported}, state}
  end

  @doc false
  def handle_cast({:board_added, board}, 
                  state = %State{table: table, position: position}) do
    table |> add_board(position, board)
    {:noreply, state |> State.next_state}
  end
  def handle_cast({:board_gone, board}, state = %State{table: table}) do
    {:removed, position} = table |> remove_board(board)
    {:noreply, state |> State.add_gap(position)}
  end 
  def handle_cast(_request, state) do
    {:noreply, state}
  end

  @doc false
  def terminate(_reason, _state) do
    self |> Gossip.unsubscribe
    # Table is automatically cleaned up.
    :ok
  end

  # Helper functions:

  defp add_board(table, position = {x, y}, new_board) do
    table |> BoardDB.add(position, new_board)
    up = table |> BoardDB.get {x, y + 1} 
    down = table |> BoardDB.get {x, y - 1}
    right = table |> BoardDB.get {x + 1, y}
    left = table |> BoardDB.get {x - 1, y}

    Board.add(up, :up_of, new_board)
    Board.add(down, :down_of, new_board)
    Board.add(right, :right_of, new_board)
    Board.add(left, :left_of, new_board)

    :ok
  end

  defp remove_board(table, board) do
    # Note: the node this board is on is already removed from the cluster!
    # This makes it impossible to simply clean up from the board process itself.
    position = table |> BoardDB.get_key(board)
    table |> do_remove(position)
  end

  defp do_remove(_table, :no_position) do
    {:removed, :no_position}
  end
  defp do_remove(table, position = {x, y}) do
    table |> BoardDB.delete(position)

    up = table |> BoardDB.get {x, y + 1} 
    down = table |> BoardDB.get {x, y - 1}
    right = table |> BoardDB.get {x + 1, y}
    left = table |> BoardDB.get {x - 1, y}

    Board.remove(:down_of, up)
    Board.remove(:up_of, down)
    Board.remove(:left_of, right)
    Board.remove(:right_of, left)

    {:removed, position}
  end

  defp to_board(node), do: {Board, node}

  defp args, do: :ok
end

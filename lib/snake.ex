defmodule Snake.Snake do
  use GenServer
  alias Snake.SnakeSupervisor
  alias Snake.Ticker
  alias Snake.Insect
  alias Snake.Board
  alias Snake.GUI
  
  # TODO check for errors (mostly move/collision part)!

  @server __MODULE__

  # TODO refactor following items to the board module
  @width 10
  @height 10

  @score 10

  defmodule State do
    defstruct segment: :head, position: {0, 0},
              next_segment: :no_pid, node: Node.self

    @doc """
    Updates the position contained in the state.
    """
    def move(state = %State{}, new_pos), do: %State{state | position: new_pos}
  end

  # API

  @doc """
  Starts a snake process (head) at location {0, 0}.
  """
  def start_link, do: start_link(:head, 0, 0)

  @doc """
  Starts a snake process (head or tail) at location {X, Y}.
  """
  def start_link(:head, x, y) do
    args = [:head, x, y]
    GenServer.start_link(__MODULE__, args)
  end
  def start_link(:tail, x, y) do
    args = [:tail, x, y]
    GenServer.start_link(__MODULE__, args)
  end

  @doc """
  Starts a snake process with a certain state
  (only used by supervisor to move snake from node to node).
  """
  def start_link([state = %State{}]) do
    args = [%State{state | node: Node.self}]
    GenServer.start_link(__MODULE__, args)
  end

  # GenServer callbacks

  @doc false
  def init([:head, x, y]) do
    snake = self
    Ticker.subscribe fn(:tick) ->
      snake |> GenServer.cast :update_state
    end
    {:ok, %State{segment: :head, position: {x, y}}}
  end
  def init([:tail, x, y]) do
    {:ok, %State{segment: :tail, position: {x, y}}}
  end
  def init([state = %State{segment: :head}]) do
    snake = self
    Ticker.subscribe fn(:tick) ->
      snake |> GenServer.cast :update_state
    end
    {:ok, state}
  end
  def init([state = %State{segment: :tail}]), do: {:ok, state}

  @doc false
  def handle_call({:is_collision, {x, y, node}}, _from,
                  state = %State{position: {x, y}, node: node}) do
    # Same coordinates and node => collision!
    {:reply, {:is_collision, true}, state}
  end
  def handle_call({:is_collision, {x, y, node}}, _from,
                  state = %State{next_segment: next}) do
    # All other cases => no collision (here), check rest!
    reply = is_collision(next, x, y, node)
    {:reply, reply, state}
  end

  def handle_call({:move, new_position, node}, _from, 
                    state = %State{next_segment: :no_pid, node: node}) do
    # TODO only called in tail segments.
    # Previous part is on same node as this one, no special action needed.
    reply = {:tail, self} # pid stays the same => return self!
    {:reply, reply, state |> State.move(new_position)}
  end
  def handle_call({:move, new_position, node}, _from,
                  state = %State{next_segment: :no_pid}) do
    # Only called in tail segments.
    # Previous part is on another node => special action needed.
    # 1) Update state.
    new_state = state |> State.move(new_position)
    # 2) Start the process on the new node with this new state.
    {:ok, new_segment} = SnakeSupervisor.start_child(node, new_state)
    # 3) Notify previous segment that this process is now on another node.
    # And finally stop this process.
    reason = :moving_to_another_node
    reply = {:tail, new_segment}
    {:stop, reason, reply, state}
  end
  def handle_call({:move, new_position, node}, _from,
                  state = %State{position: old_position, 
                                next_segment: next,
                                node: node}) do
    # Only called in tail segments.
    # Previous part is on same node as this one, no special action needed.
    {:tail, segment} = next |> GenServer.call {:move, old_position, node}
    new_state = %State{state | next_segment: segment, position: new_position}
    reply = {:tail, self}
    {:reply, reply, new_state}
  end
  def handle_call({:move, new_position, new_node}, _from,
                  state = %State{position: old_position, 
                                  next_segment: next}) do
    # Only called in tail segments.
    # Previous part is on another node => special action needed.
    # 1) Notify next segment of movement
    {:tail, segment} = next |> GenServer.call {:move, old_position, Node.self}
    # 2) Update state
    new_state = %State{state | position: new_position, next_segment: segment}
    # 3) Start the process on the new node with this new state.
    {:ok, new_segment} = SnakeSupervisor.start_child(new_node, new_state)
    # 4) Update previous segment that this segment is now on another node.
    reason = :moving_to_other_node
    reply = {:tail, new_segment}
    {:stop, reason, reply, new_state}
  end
  def handle_call(_request, _from, state) do 
    {:reply, {:error, :not_supported}, state}
  end

  @doc false
  def handle_cast(:update_state, state = %State{}) do
    # Only triggered in the head! => updates the entire snake recursively!
    # Ask for new input and move snake
    Input.get_direction 
      |> move(state)
      |> handle_move_result
    # TODO merge these 2 casts back together!
  end
  def handle_cast(:update_state2, state = %State{position: {new_x, new_y}, 
                                                  next_segment: next,
                                                  node: node}) do
    # Check for collisions with itself
    next 
      |> is_collision(new_x, new_y, node)
      |> handle_collision
    
    # Check for collisions with insect => if yes: update score
    Insect.get_location
      |> is_insect_collision(state)
      |> update_score

    # Draw the snake
    state |> draw
    {:noreply, state}
  end
  def handle_cast(:draw, state = %State{}) do
    state |> draw
    {:noreply, state}
  end
  def handle_cast(:gameover, state = %State{next_segment: :no_pid}) do
    {:stop, :gameover, state}
  end
  def handle_cast(:gameover, state = %State{next_segment: next}) do
    next |> GenServer.cast :gameover
    {:stop, :gameover, state}
  end
  def handle_cast(_request, state) do
    {:noreply, state}
  end

  @doc false
  def terminate(_reason, %State{segment: :head}) do
    self |> Ticker.unsubscribe 
    :ok
  end
  def terminate(_reason, _state), do: :ok

  # Helper functions

  # GUI related

  defp update_score(:no_collision), do: :ok
  defp update_score(:collision), do: GUI.update_score(%{score: @score})

  defp draw(%State{position: {x, y}, node: a_node, next_segment: :no_pid}) do
    # TODO maybe work with diff here, draw it on next x,y 
    # but clear previous rectangle => lock canvas during each draw?
    draw_segment(x, y, a_node)
  end
  defp draw(%State{position: {x, y}, node: a_node, next_segment: next}) do
    draw_segment(x, y, a_node)
    next |> GenServer.cast :draw # TODO make this a call?
  end

  defp draw_segment(x, y, a_node) do
    GUI.draw_snake(%{x: x, y: y, node: a_node, color: color})
  end

  # Other helpers

  defp move(direction, state = %State{next_segment: :no_pid}) do
    do_move(direction, state) # TODO is this case even used?
  end
  defp move(direction, state = %State{position: {old_x, old_y},
                                      next_segment: next,
                                      node: node}) do
    {:tail, segment} = next |> GenServer.call {:move, {old_x, old_y}, node}
    new_state = %State{state | next_segment: segment}
    do_move(direction, new_state)
  end

  defp do_move(:left, state = %State{position: {x, y}}) when x > 0 do
    {:noreply, %State{state | position: {x - 1, y}}}
  end
  defp do_move(:left, state = %State{position: {_x, y}}) do
    {Board, new_node} = Board.get(:left)
    new_state = %State{state | position: {@width, y}}
    new_head = spawn_head(new_node, new_state)
    {:stop, {:moving_to_another_node, new_head}, state}
  end
  defp do_move(:right, state = %State{position: {x, y}}) when x < @width do
    {:noreply, %State{state | position: {x + 1, y}}}
  end
  defp do_move(:right, state = %State{position: {_x, y}}) do
    {Board, new_node} = Board.get(:right)
    new_state = %State{state | position: {0, y}}
    new_head = spawn_head(new_node, new_state)
    {:stop, {:moving_to_another_node, new_head}, state}
  end
  defp do_move(:up, state = %State{position: {x, y}}) when y < @height do
    {:noreply, %State{state | position: {x, y + 1}}}
  end
  defp do_move(:up, state = %State{position: {x, _y}}) do
    {Board, new_node} = Board.get(:up)
    new_state = %State{state | position: {x, 0}}
    new_head = spawn_head(new_node, new_state)
    {:stop, {:moving_to_another_node, new_head}, state}
  end
  defp do_move(:down, state = %State{position: {x, y}}) when y > 0 do
    {:noreply, %State{state | position: {x, y - 1}}}
  end
  defp do_move(:down, state = %State{position: {x, _y}}) do
    {Board, new_node} = Board.get(:down)
    new_state = %State{state | position: {x, @height}}
    new_head = spawn_head(new_node, new_state)
    {:stop, {:moving_to_another_node, new_head}, state}
  end

  defp spawn_head(:no_board, _state) do
    self |> GenServer.cast :gameover # TODO make this a call, other the {stop, ...} prevents game from cleaning up?
  end
  defp spawn_head({:board, node}, state) do
    {:ok, head} = SnakeSupervisor.start_child(node, state)
    head
  end

  defp is_insect_collision({x, y, node}, %State{position: {x, y}, 
                                                node: node}) do
    :collision
  end
  defp is_insect_collision(_insect_location, _snake_state) do
    :no_collision
  end

  defp handle_move_result(result = {:noreply, _state}) do
    self |> GenServer.cast :update_state2
    result
  end
  defp handle_move_result(result = {:stop, {_reason, new_head}, _state}) do
    new_head |> GenServer.cast :update_state2
    result
  end

  defp is_collision(:no_pid, _x, _y, _node), do: {:is_collision, false}
  defp is_collision(next, x, y, node) do
    # TODO maybe use cast here?
    # splits game logic up in 2 parts though..
    next |> GenServer.cast {:is_collision, {x, y, node}}
  end

  defp handle_collision({:is_collision, false}), do: :ok
  defp handle_collision({:is_collision, true}) do
    self |> GenServer.cast :gameover
  end
  
  defp color do
    colors = [:orange, :green, :blue, :cyan, :yellow, :purple, :white]
    length = 7
    pick_random colors, length
  end

  defp pick_random(list, length_of_list) do
    # Generate random number:
    <<a :: 32, b :: 32, c :: 32>> = :crypto.rand_bytes(12)
    :random.seed {a, b, c}
    
    random_index = Float.round(:random.uniform * length_of_list)
    list |> Enum.at random_index
  end
end

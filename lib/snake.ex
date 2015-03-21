defmodule Snake.Snake do
  use GenServer
  use Snake.Game
  alias Snake.Ticker
  alias Snake.Insect
  alias Snake.Board
  alias Snake.Tile
  alias Snake.TileDB
  alias Snake.Random
  alias Snake.GUI

  @moduledoc """
  Module containing logic for the snake process.

  TODO:
  =====
  - gameover => simply crash
  - (re)start game + reacting to player input
  """

  @server {:global, __MODULE__} # TODO or local registered process?

  defmodule State do
    use Snake.Game

    @moduledoc """
    Module for the state of the snake. Also contains a few helper functions.
    """

    @default_value quote do: throw "Unassigned field in struct!"
    defstruct size: @default_value,
              positions: @default_value,
              previous_positions: @default_value,
              direction: @default_value,
              current_direction: @default_value,
              color: @default_value
    
    @doc """
    Creates a snake of size 'size' at a random {x, y} on a random node.
    """
    def new(size) do
      Random.generate_seed

      # First x and y somewhere in the middle of the board.
      first_x = round((Random.number(4) - 2) + 0.5 * width)
      first_y = round((Random.number(4) - 2) + 0.5 * height)
      first_node = Random.element [Node.self | Node.list]
      direction = Random.element [:up, :down, :left, :right]
      color = Random.element [:orange, :green, :blue, :cyan, 
                                :yellow, :purple, :white]

      tail = calc_tail_positions(first_x, first_y, first_node, 
                                  direction, size - 1, [])
      positions = [{first_x, first_y, first_node} | tail]

     state = %State{size: size, 
                    positions: positions, previous_positions: positions,
                    direction: direction, current_direction: direction,
                    color: color}
      
                  # Check if insect exists, prevent collisions
      Insect 
      |> :global.whereis_name 
      |> check_for_insect_collisions(state)
    end

    defp check_for_insect_collisions(:undefined, state), do: state
    defp check_for_insect_collisions(_insect, state) do
      {insect_x, insect_y, insect_node} = Insect.get_position
      state.positions |> Enum.filter(fn({x, y, n}) ->
        x == insect_x && y == insect_y && n == insect_node
      end)
      |> handle_collision_result(state)
    end

    defp handle_collision_result([], state), do: state
    defp handle_collision_result(_, state),  do: new(state.size)

    @doc """
    Moves the snake.
    """
    def move(state = %State{positions: [{x, y, a_node} | _tail], 
                            direction: :left}) when x > 0 do
      state 
      |> update_current_direction
      |> do_move(x - 1, y, {Board, a_node})
    end
    def move(state = %State{positions: [{_x, y, _node} | _tail], 
                            direction: :left}) do
      board = Board.get(:left)
      state
      |> update_current_direction
      |> do_move(width, y, board)
    end
    def move(state = %State{positions: [{x, y, a_node} | _tail], 
                            direction: :right}) when x < width - 1 do
      state
      |> update_current_direction
      |> do_move(x + 1, y, {Board, a_node})
    end
    def move(state = %State{positions: [{_x, y, _node} | _tail], 
                            direction: :right}) do
      board = Board.get(:right)
      state
      |> update_current_direction
      |> do_move(0, y, board)
    end
    def move(state = %State{positions: [{x, y, a_node} | _tail], 
                            direction: :up}) when y < height - 1 do
      state
      |> update_current_direction
      |> do_move(x, y + 1, {Board, a_node})
    end
    def move(state = %State{positions: [{x, _y, _node} | _tail], 
                            direction: :up}) do
      board = Board.get(:up)
      state 
      |> update_current_direction
      |> do_move(x, 0, board)
    end
    def move(state = %State{positions: [{x, y, a_node} | _tail], 
                            direction: :down}) when y > 0 do
      state 
      |> update_current_direction
      |> do_move(x, y - 1, {Board,a_node})
    end
    def move(state = %State{positions: [{x, _y, _node} | _tail], 
                            direction: :down}) do
      board = Board.get(:down)
      state 
      |> update_current_direction
      |> do_move(x, height, board)
    end

    @doc """
    Appends a segment to the end of the snake (same location as previous 
    last segment).
    """
    def add_segment(state = %State{positions: positions}) do
      reverse_pos = Enum.reverse positions
      new_positions = Enum.reverse [hd(reverse_pos) | reverse_pos]
      %State{state | size: state.size + 1, positions: new_positions}
    end

    # Helper functions

    defp update_current_direction(state = %State{direction: dir}) do
      %State{state | current_direction: dir}
    end

    defp do_move(state = %State{}, _x, _y, :no_board), do: {:gameover, state}
    defp do_move(state = %State{positions: pos}, new_x, new_y, 
                  {Board, new_node}) do
      # Drops last segment, 
      # adds new segment to beginning (new location for head)
      # and updates the state.
      {:ok, %State{state 
             | positions: [{new_x, new_y, new_node} | Enum.drop(pos, -1)],
              previous_positions: pos}}
    end

    defp calc_tail_positions(_x, _y, _node, _dir, 0, acc) do
      Enum.reverse acc
    end
    defp calc_tail_positions(x, y, a_node, :left, amount, acc) do
      calc_tail_positions(x + 1, y, a_node, :left, amount - 1, 
                          [{x + 1, y, a_node} | acc])  
    end
    defp calc_tail_positions(x, y, a_node, :right, amount, acc) do
      calc_tail_positions(x - 1, y, a_node, :right, amount - 1,
                          [{x - 1, y, a_node} | acc])
    end
    defp calc_tail_positions(x, y, a_node, :up, amount, acc) do
      calc_tail_positions(x, y - 1, a_node, :up, amount - 1,
                          [{x, y - 1, a_node}| acc])
    end
    defp calc_tail_positions(x, y, a_node, :down, amount, acc) do
      calc_tail_positions(x, y + 1, a_node, :down, amount - 1,
                          [{x, y + 1, a_node} | acc])
    end
  end


  # Snake API

  @doc """
  Starts the snake process (size = 4 blocks) at a random location {x,y}.
  """
  def start_link, do: start_link(3)

  @doc """
  Starts a snake of a certain size at a random location {x, y}.
  """
  def start_link(size) when is_integer(size) and size > 0 do
    GenServer.start_link(__MODULE__, State.new(size), [name: @server])
  end

  @doc """
  Returns a list of positions for each segment of the snake.
  """
  def get_positions do
    @server |> GenServer.call :get_positions
  end

  @doc """
  Updates the direction the snake will move to next.
  """
  def set_direction("up"),    do: update_direction(:up)
  def set_direction("down"),  do: update_direction(:down)
  def set_direction("left"),  do: update_direction(:left)
  def set_direction("right"), do: update_direction(:right)
 
  @doc """
  Subscribes the snake to the ticker process.
  """
  def subscribe_to_ticker do
    snake = :global.whereis_name __MODULE__
    Ticker.subscribe snake, fn(:tick) ->
      snake |> update_state
    end
  end

  @doc """
  Force the snake to redraw its current state
  """
  def draw do
    @server |> GenServer.call :draw
  end

  # GenServer callbacks

  @doc false
  def init(state = %State{positions: positions, color: color}) do
    positions |> Enum.map fn({x, y, a_node}) ->
      tile_notify_arrived(x, y, a_node, color)
    end

    # TODO also subscribe to insect for events!
    {:ok, state}
  end

  @doc false
  def handle_call(:get_positions, _from, state = %State{positions: pos}) do
    {:reply, pos, state}
  end
  def handle_call(:update_state, _from, state = %State{}) do
    {status, new_state} = 
      state
      |> State.move                   # 1) Move the snake
      |> check_for_snake_collisions   # 2) Check for collisions with itself
      |> check_for_insect_collisions  # 3) Check for collisions with insect
      |> draw_snake(state)            # 4) Update GUI

    {status, new_state} |> handle_status
    # TODO handle gameover better? TODO make supervisors permanent for board, game and tile!
  end
  def handle_call({:update_direction, :left}, _from, 
                  state = %State{current_direction: :right}) do
                    # Do not allow this (would instantly cause gameover),
                    # same for other clauses..
    {:reply, :ok, state}
  end
  def handle_call({:update_direction, :right}, _from, 
                  state = %State{current_direction: :left}) do
    {:reply, :ok, state}
  end
  def handle_call({:update_direction, :up}, _from, 
                    state = %State{current_direction: :down}) do
    {:reply, :ok, state}
  end
  def handle_call({:update_direction, :down}, _from, 
                  state = %State{current_direction: :up}) do
    {:reply, :ok, state}
  end
  def handle_call({:update_direction, new_dir}, _from, state = %State{}) do
    {:reply, :ok, %State{state | direction: new_dir}}
  end
  def handle_call(:draw, _from, state = %State{positions: pos, color: c}) do
    pos |> Enum.map fn({x, y, n}) ->
      GUI.draw_snake %{x: x, y: y, node: n, color: c}
    end
    {:reply, :ok, state}
  end
  def handle_call(_request, _from, state = %State{}) do
    {:reply, {:error, :not_supported}, state}
  end

  @doc false
  def terminate(_reason, %State{positions: positions, 
                                previous_positions: old_positions}) do
    Ticker.unsubscribe self
    insect = :global.whereis_name Insect
    Ticker.unsubscribe insect
    
    positions |> Enum.map fn({x, y, a_node}) ->
      tile_notify_gone(x, y, a_node)
    end

    old_positions |> Enum.map fn({x, y, a_node}) ->
      tile_notify_gone(x, y, a_node)
    end

    GUI.reset_score
    :ok
  end
  def terminate(_reason, state) do
    IO.inspect state
    :ok
  end


  # Helper functions

  defp check_for_snake_collisions(arg = {:gameover, _}), do: arg
  defp check_for_snake_collisions({:ok, 
                                  state = %State{positions: [head | tail]}}) do
    # Checks for collisions with itself
    handle_collision_result(head in tail, state)
  end

  defp handle_collision_result(false, state), do: {:ok, state}
  defp handle_collision_result(true, state), do:  {:gameover, state}

  defp check_for_insect_collisions(arg = {:gameover, _}), do: arg
  defp check_for_insect_collisions({:ok, 
                                    state = %State{positions: [head | _]}}) do
    handle_insect_collision_result(head, Insect.get_position, state)
  end
  
  defp handle_insect_collision_result(position, position, 
                                      state = %State{positions: positions}) do
    Insect.kill(positions) #  TODO refactor later (use agent!)
    GUI.update_score %{score: score}
    {:ok, state |> State.add_segment}
  end
  defp handle_insect_collision_result(_position1, _position2, state) do
    {:ok, state}
  end

  defp draw_snake(arg = {:gameover, _}, _state), do: arg
  defp draw_snake({:ok, new_state = %State{positions: new_positions, 
                                            color: color}}, 
                 _old_state = %State{positions: old_positions}) do
    # Only update first and last part of snake
    [{newest_x, newest_y, newest_node} | _] = new_positions
    [{last_x, last_y, last_node} |_] = Enum.reverse(old_positions)

    tile_notify_gone(last_x, last_y, last_node)
    tile_notify_arrived(newest_x, newest_y, newest_node, color)

    {:ok, new_state}
  end

  defp handle_status({:gameover, new_state}) do
    {:stop, :gameover, :ok, new_state}
  end
  defp handle_status({:ok, new_state}) do
    {:reply, :ok, new_state}
  end

  defp tile_notify_arrived(x, y, a_node, color) do
    snake = self
    Node.spawn a_node, fn ->
      {x, y}
      |> TileDB.get
      |> Tile.notify_arrival(:snake, snake, color)
    end
  end

  defp tile_notify_gone(x, y, a_node) do
    snake = self
    Node.spawn a_node, fn ->
      {x, y}
      |> TileDB.get
      |> Tile.notify_gone(snake)
    end
  end

  defp update_state(snake), do: :ok = snake |> GenServer.call :update_state
  defp update_direction(direction) do
    @server |> GenServer.call {:update_direction, direction}
  end
end

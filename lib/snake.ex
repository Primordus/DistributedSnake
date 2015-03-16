defmodule Snake.Snake do
  use GenServer
  use Snake.Game
  alias Snake.SnakeSupervisor, as: SnakeSup
  alias Snake.Ticker
  alias Snake.Insect
  alias Snake.Board
  alias Snake.Tile
  alias Snake.TileDB
  alias Snake.Random

  @moduledoc """
  Module containing logic for the snake process.

  TODO:
  =====
  - subscribe to input
  - gameover => simply crash
  - (re)start game + reacting to player input
  - unit tests + docs
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
              direction: @default_value,
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

      %State{size: size, positions: positions, 
              direction: direction, color: color}
    end

    @doc """
    Moves the snake.
    """
    def move(state = %State{positions: pos = [{x, y, a_node} | _tail], 
                            direction: :left}) when x > 0 do
      state |> do_move(x - 1, y, a_node)
    end
    def move(state = %State{positions: pos = [{_x, y, _node} | _tail], 
                            direction: :left}) do
      {Board, new_node} = Board.get(:left)
      state |> do_move(width, y, new_node)
    end
    def move(state = %State{positions: pos = [{x, y, a_node} | _tail], 
                            direction: :right}) when x < width do
      state |> do_move(x + 1, y, a_node)
    end
    def move(state = %State{positions: pos = [{_x, y, _node} | _tail], 
                            direction: :right}) do
      {Board, new_node} = Board.get(:right)
      state |> do_move(0, y, new_node)
    end
    def move(state = %State{positions: pos = [{x, y, a_node} | _tail], 
                            direction: :up}) when y < height do
      state |> do_move(x, y + 1, a_node)
    end
    def move(state = %State{positions: pos = [{x, _y, _node} | _tail], 
                            direction: :up}) do
      {Board, new_node} = Board.get(:up)
      state |> do_move(x, 0, new_node)
    end
    def move(state = %State{positions: pos = [{x, y, a_node} | _tail], 
                            direction: :down}) when y > 0 do
      state |> do_move(x, y - 1, a_node)
    end
    def move(state = %State{positions: pos = [{x, _y, _node} | _tail], 
                            direction: :down}) do
      {Board, new_node} = Board.get(:down)
      state |> do_move(x, height, new_node)
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

    defp do_move(state = %State{positions: pos}, new_x, new_y, new_node) do
      # Drops last segment, 
      # adds new segment to beginning (new location for head)
      # and updates the state.
      %State{state | positions: [{new_x, new_y, new_node} | Enum.drop(pos, -1)]}
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

  
  # GenServer callbacks

  @doc false
  def init(state = %State{positions: positions, color: color}) do
    snake = self
    Ticker.subscribe snake, fn(:tick) ->
      snake |> update_state
    end

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
    new_state = 
      state
      |> State.move                   # 1) Move the snake
      |> check_for_snake_collisions   # 2) Check for collisions with itself
      |> check_for_insect_collisions  # 3) Check for collisions with insect
      |> draw_snake(state)            # 4) Update GUI

    # TODO handle gameover better?
    {:reply, :ok, new_state}
  end
  def handle_call({:update_direction, new_dir}, _from, state = %State{}) do
    {:reply, :ok, %State{state | direction: new_dir}}
  end
  def handle_call(_request, _from, state = %State{}) do
    {:reply, {:error, :not_supported}, state}
  end

  @doc false
  def terminate(_reason, state = %State{positions: positions}) do
    Ticker.unsubscribe self
    
    positions |> Enum.map fn({x, y, a_node}) ->
      tile_notify_gone(x, y, a_node)
    end
    
    # TODO reset score

    :ok
  end


  # Helper functions

  defp check_for_snake_collisions(state = %State{positions: [head | tail]}) do
    # Checks for collisions with itself
    handle_collision_result(head in tail, state)
  end

  defp handle_collision_result(false, state), do: {:no_collision, state}
  defp handle_collision_result(true, state), do:  {:collision, state}

  defp check_for_insect_collisions({:no_collision, 
                                    state = %State{positions: [head | _]}}) do
    handle_insect_collision_result(head, Insect.get_position, state)
  end
  
  defp handle_insect_collision_result(position, position, state) do
    Insect.kill
    GUI.update_score %{score: score}
    state |> State.add_segment
  end
  defp handle_insect_collision_result(_position1, _position2, state), do: state

  def draw_snake(new_state = %State{positions: new_positions, color: color}, 
                  state = %State{positions: old_positions}) do
    old_positions |> Enum.map fn({x, y, a_node}) ->
      tile_notify_gone(x, y, a_node)
    end

    new_positions |> Enum.map fn({x, y, a_node}) ->
      tile_notify_arrived(x, y, a_node, color)
    end

    new_state
  end

  defp tile_notify_arrived(x, y, a_node, color) do
    snake = self
    Node.spawn_link a_node, fn ->
      {x, y}
      |> TileDB.get
      |> Tile.notify_arrival(:snake, snake, color)
    end
  end

  defp tile_notify_gone(x, y, a_node) do
    snake = self
    Node.spawn_link a_node, fn ->
      {x, y}
      |> TileDB.get
      |> Tile.notify_gone(snake)
    end
  end

  defp update_state(snake), do: :ok = snake |> GenServer.call :update_state
  defp update_direction(direction) do
    :ok = @server |> GenServer.call :update_direction, direction
  end
end

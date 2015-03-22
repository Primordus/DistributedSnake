defmodule Snake.Insect do
  use GenServer
  use Snake.Game
  alias Snake.InsectSupervisor
  alias Snake.Ticker
  alias Snake.Tile
  alias Snake.TileDB, as: TileDB
  alias Snake.Random
  alias Snake.GUI
  alias Snake.Snake # put this alias last, transforms all others!
  alias GPIO.LED

  @moduledoc """
  Module that contains the logic for the insect process.
  """

  @server {:global, __MODULE__}
  @default_counter 30

  defmodule State do
    @default_value quote do: throw "Invalid value for state!"
    defstruct x: @default_value, 
              y: @default_value, 
              node: @default_value,
              led: @default_value, 
              counter: @default_value
  end


  # API

  @doc """
  Starts a insect process at location {x, y} on this node.
  """
  def start_link(led, _args = %{x: x, y: y}) do
    GenServer.start_link(__MODULE__, 
                        %{x: x, y: y, node: Node.self, led: led}, 
                        [name: @server])
  end

  @doc """
  Gives back the position of the insect process. 
  Return value is a tuple of the following form: {x, y, node}.
  """
  def get_position, do: @server |> GenServer.call :get_position

  @doc """
  Kills the current insect.
  """
  def kill(snake_pos) do
    :ok = @server |> GenServer.call {:kill, {:snake_positions, snake_pos}}
  end

  @doc """
  Subscribes the insect to events from the ticker process
  """
  def subscribe_to_ticker do
    insect = :global.whereis_name __MODULE__
    Ticker.subscribe insect, fn(:tick) ->
      insect |> update_state
    end
  end

  @doc """
  Force the insect to draw its current state.
  """
  def draw do
    @server |> GenServer.call :draw
  end

  # GenServer callbacks

  @doc false
  def init(%{x: x, y: y, led: led}) do
    Random.generate_seed  
    n = Node.self
    tile_notify_arrived(x, y, n)
    {:ok, %State{x: x, y: y, node: n, led: led, counter: @default_counter}}
  end

  @doc false
  def handle_call(:get_position, _from, state = %State{x: x, y: y, node: n}) do
    {:reply, {x, y, n}, state}
  end
  def handle_call({:kill, {:snake_positions, snake_pos}},
                  _from, state = %State{led: led}) do
    #led |> LED.pulse
    {:reply, :ok, state |> new_insect(snake_pos)}
  end
  def handle_call(:update_state, _from, state = %State{counter: 0}) do
    #state |> new_insect
    snake_positions = Snake 
    |> :global.whereis_name # TODO refactor later 
    |> handle_snake_alive 
    {:reply, :ok, state |> new_insect(snake_positions)}
  end
  def handle_call(:update_state, _from, state = %State{counter: counter}) do
    {:reply, :ok, %State{state | counter: counter - 1}}
  end
  def handle_call(:draw, _from, state = %State{x: x, y: y, node: n}) do
    GUI.draw_insect %{x: x, y: y, node: n}
    {:reply, :ok, state}
  end
  def handle_call(_request, _from, state) do
    {:reply, {:error, :not_allowed}, state}
  end

  defp handle_snake_alive(:undefined), do: []
  defp handle_snake_alive(_),          do: Snake.get_positions

  @doc false
  def terminate(_reason, _state) do
    self |> Ticker.unsubscribe
    :ok
  end

  # Helper functions

  defp new_insect(%State{x: x, y: y, node: n, led: led}, snake_positions) do
    {new_x, new_y, new_node} = find_new_position(snake_positions)
   
    tile_notify_gone(x, y, n) 
    tile_notify_arrived(new_x, new_y, new_node)
    %State{x: new_x, y: new_y, led: led,
            node: new_node, counter: @default_counter}


    #:global.unregister_name __MODULE__  # TODO refactor this later
    #{:ok, insect} = InsectSupervisor.start_child(%{x: new_x, y: new_y, node: new_node})
    #Ticker.subscribe insect, fn(:tick) ->
    #  insect |> update_state
    #end
  end

  defp find_new_position(snake_positions) do
    random_x = Random.number(width)
    random_y = Random.number(height)
    random_node = [Node.self | Node.list] |> Random.element

    snake_positions 
    |> Enum.filter(fn({x, y, a_node}) ->
      x == random_x && y == random_y && a_node == random_node
    end)
    |> check_for_collision
    |> handle_collision(snake_positions, {random_x, random_y, random_node})
  end

  defp check_for_collision([]), do: :no_collision
  defp check_for_collision([{_x, _y, _node}]), do: :collision

  defp handle_collision(:collision, snake_positions, _position) do
    find_new_position(snake_positions)
  end
  defp handle_collision(:no_collision, _snake_positions, position) do
    position
  end

  defp tile_notify_arrived(x, y, n) do
    insect = self
    Node.spawn n, fn ->
      {x, y}
      |> TileDB.get
      |> Tile.notify_arrival(:insect, insect)
    end
  end

  defp tile_notify_gone(x, y, n) do
    insect = self
    Node.spawn n, fn ->
      {x, y} 
      |> TileDB.get
      |> Tile.notify_gone(insect)
    end
  end

    defp update_state(insect), do: :ok = insect |> GenServer.call :update_state
end

defmodule Snake.Insect do
  use GenServer
  use Snake.Game
  alias Snake.InsectSupervisor
  alias Snake.Ticker
  alias Snake.Tile
  alias Snake.TileDB, as: TileDB
  alias Snake.Random
  alias Snake.Snake # put this alias last, transforms all others!
  alias GPIO.LED

  @moduledoc """
  Module that contains the logic for the insect process.
  """

  @server {:global, __MODULE__}
  @default_counter 10

  defmodule State do
    @default_value quote do: throw "Invalid value for state!"
    defstruct x: @default_value, 
              y: @default_value, 
              led: @default_value, 
              counter: @default_value
  end


  # API

  @doc """
  Starts a insect process at location {x, y} on this node.
  """
  def start_link(led, _args = %{x: x, y: y}) do
    GenServer.start_link(__MODULE__, %{x: x, y: y, led: led}, [name: @server])
  end

  @doc """
  Gives back the location of the insect process. 
  Return value is a tuple of the following form: {x, y, node}.
  """
  def get_location, do: @server |> GenServer.call :get_location

  @doc """
  Kills the current insect.
  """
  def kill, do: :ok = @server |> GenServer.call :kill


  # GenServer callbacks

  @doc false
  def init(%{x: x, y: y, led: led}) do
    insect = self
    Ticker.subscribe insect, fn(:tick) ->
      insect |> update_state
    end

    tile_notify_arrived(x, y)
    Random.generate_seed 
    
    {:ok, %State{x: x, y: y, led: led, counter: @default_counter}}
  end

  @doc false
  def handle_call(:get_location, _from, state = %State{x: x, y: y}) do
    {:reply, {x, y, node}, state}
  end
  def handle_call(:kill, _from, state = %State{led: led}) do
    led |> LED.pulse
    state |> spawn_new_insect
    {:stop, :normal, :ok, state}
  end
  def handle_call(:update_state, _from, state = %State{counter: 0}) do
    state |> spawn_new_insect
    {:stop, :normal, :ok, state}
  end
  def handle_call(:update_state, _from, state = %State{counter: counter}) do
    {:reply, :ok, %State{state | counter: counter - 1}}
  end

  @doc false
  def terminate(_reason, _state) do
    self |> Ticker.unsubscribe
    :ok
  end

  # Helper functions

  defp spawn_new_insect(state = %State{x: x, y: y}) do
    snake_positions = Snake.get_positions
    {new_x, new_y, new_node} = find_new_position(snake_positions)
   
    # TODO try to put the following lines in terminate function?
    tile_notify_gone(x, y) 
    :global.unregister_name __MODULE__  # TODO refactor this later
    InsectSupervisor.start_child(%{x: new_x, y: new_y, node: new_node})
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

  defp tile_notify_arrived(x, y) do
    {x, y}
    |> TileDB.get
    |> Tile.notify_arrival(:insect, self)
  end

  defp tile_notify_gone(x, y) do
    {x, y} 
    |> TileDB.get
    |> Tile.notify_gone(self)
  end

    defp update_state(insect), do: :ok = insect |> GenServer.call :update_state
end

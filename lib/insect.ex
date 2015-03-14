defmodule Snake.Insect do
  use GenServer
  use Snake.Game
  alias Snake.Ticker
  alias Snake.Snake
  alias Snake.InsectSupervisor
  alias Snake.GUI
  alias GPIO.LED

  @server __MODULE__
  @default_counter 10
  @max 10

  defmodule State do
    defstruct x: 0, y: 0, led: :no_pid, counter: 10, alive: false
  end

  # API

  def start_link(args = %{x: _x, y: _y}) do
    GenServer.start_link(__MODULE__, args) # TODO register?
  end

  def get_location(pid) do
    pid |> GenServer.call :get_location
  end

  def kill(insect) do
    insect |> GenServer.call :kill
  end

  # GenServer callbacks

  def init(%{x: x, y: y}) do
    insect = self
    Ticker.subscribe insect, fn(:tick) ->
      insect |> update_state
    end

    # Generate random seed:
    <<a :: 32, b :: 32, c :: 32>> = :crypto.rand_bytes(12)
    :random.seed {a, b, c}

    pin = 18
    {:ok, led} = LED.start_link(pin)
    
    {:ok, %State{x: x, y: y, led: led}}
  end

  def handle_call(:get_location, _from, state = %State{x: x, y: y}) do
    reply = {:location, {x, y, node}}
    {:reply, reply, state}
  end

  def handle_call(:update_state, _from, state = %State{x: x, y: y, 
                                                        counter: 0}) do
    random_node = [Node.self | Node.list] |> random_element
    random_x = random_number(@max)
    random_y = random_number(@max)

    if random_node == Node.self do 
        {:reply, :ok, %State{state | x: random_x, y: random_y, counter: @default_counter}}
    else
        # Other node!
        
    end
  end
  def handle_call(:update_state, _from, state = %State{x: x, y: y, 
                                                        counter: counter}) do
    {:reply, :ok, %State{state | counter: counter - 1}}
  end
  def handle_call(:kill, _from, state = %State{led: led}) do
    # Score updaten + IO aansturen!
    led |> LED.pulse
    {:stop, :normal, :ok, state}
  end

  # Helper functions

  defp active? do

  end

  defp random_number(max) do
    Float.floor(:random.uniform * max)
  end

  defp random_element(list) do
    random_element(list, length(list))
  end

  defp random_element(list, length) do
    random_index = Float.floor(:random.uniform * length)
    list |> Enum.at random_index
  end

  defp update_state(insect) do
    :ok = insect |> GenServer.call :update_state
  end
end

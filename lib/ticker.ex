defmodule Snake.Ticker do
  use GenServer
  alias Snake.PubSub

  @moduledoc """
  Process that emits a tick to a pubsub mechanism every X milliseconds.
  """

  @server {:global, __MODULE__}
  @default_delay 1000 # 1 second.

  defmodule State, do: defstruct delay: 0, pubsub: :no_pid

  # API
  
  @doc """
  Starts a ticker process with the default delay (1 second).
  """
  def start_link, do: start_link @default_delay

  @doc """
  Starts a ticker process with a certain delay in milliseconds.
  """
  def start_link(delay) when delay > 0 do
    args = {:delay, delay}
    GenServer.start_link(__MODULE__, args, [name: @server]) # TODO check what this gives when started on 2nd node => process result
  end

  @doc """
  Subscribes a process to the ticker process to start receiving ticks.
  """
  def subscribe(pid, sub_function) when is_function(sub_function, 1) do
    @server |> GenServer.cast {:subscribe, pid, sub_function}
  end

  @doc """
  Unsubscribes a process from the ticker process.
  """
  def unsubscribe(pid) when is_pid(pid) do
    @server |> GenServer.cast {:unsubscribe, pid}
  end

  # GenServer callbacks

  @doc false
  def init({:delay, delay}) do
    Process.flag(:trap_exit, true)

    {:ok, pubsub} = PubSub.start_link
    spawn_worker(delay)
    {:ok,  %State{delay: delay, pubsub: pubsub}}
  end

  @doc false
  def handle_cast({:subscribe, pid, sub_func}, 
                  state = %State{pubsub: pubsub}) do
    pubsub |> PubSub.add_sub(pid, sub_func)
    {:noreply, state}
  end
  def handle_cast({:unsubscribe, pid}, state = %State{pubsub: pubsub}) do
    pubsub |> PubSub.remove_sub(pid)
    {:noreply, state}
  end
  def handle_cast(:time_expired, 
                  state = %State{delay: delay, pubsub: pubsub}) do
    pubsub |> PubSub.publish :tick                
    spawn_worker(delay)
    {:noreply, state}
  end

  # Helper functions
  
  defp spawn_worker(delay) do
    spawn fn ->
      :timer.sleep(delay)
      @server |> GenServer.cast :time_expired
    end
  end
end

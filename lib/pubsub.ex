defmodule Snake.Pubsub do
  use GenEvent

  @moduledoc """
  Simple publish subscribe mechanism.
  """

  @doc """
  Starts the event manager for the pubsub mechanism (can be outside of a
  supervision tree.
  """
  def start_link do
    {:ok, manager} = GenEvent.start_link
    args = :ok
    manager |> GenEvent.add_handler __MODULE__, args
    {:ok, manager}
  end

  @doc """
  Publishes a message to all subscribers of the pubsub event manager.
  """
  def publish(manager, msg) when is_pid(manager) do
    manager |> GenEvent.sync_notify {:publish, msg}
  end

  @doc """
  Adds a function that can process an incoming message as a subscriber to the 
  pubsub mechanism. This can be used to e.g. better isolate your code in a 
  single module. The subscribe function itself should do as little work as 
  possible (for example forwarding a message).

  Example:
 
  ... 
  pid = self()
  manager |> Pubsub.add_sub fun(msg) -> 
    :ok = pid |> GenServer.call {new_msg, msg} 
  end
  ...
 
  def handle_call({new_msg, msg}, from, state) -> ...
  """
  def add_sub(manager, pid, sub_function)
      when is_pid(manager)
      and is_pid(pid)
      and is_function(sub_function, 1) do
    manager |> GenEvent.sync_notify {:add_sub, pid, sub_function}
  end

  @doc """
  Adds the calling process as a subscriber to the pubsub mechanism.
  Incoming messages are delivered as is (using send operator).
  """
  def add_sub(manager, pid) do
    manager |> add_sub fn(msg) ->
      pid |> send msg
    end
  end

  @doc """
  Removes a process from the pubsub mechanism
  """
  def remove_sub(manager, pid) when is_pid(manager) do
    manager |> GenEvent.sync_notify {:remove_sub, pid}
  end

  @doc """
  Stops the pubsub manager process.
  """
  def stop(manager) when is_pid(manager), do: manager |> GenEvent.stop

  # GenEvent callbacks

  @doc false
  def init(:ok), do: {:ok, HashDict.new} # Begin state = empty hashdict.

  @doc false
  def handle_event({:publish, msg}, subscribers = %HashDict{}) do 
    subscribers |> Enum.map fn {_pid_key, function} ->
      function |> apply [msg]
    end
    {:ok, subscribers}
  end
  def handle_event({:add_sub, pid_key, func}, subscribers = %HashDict{}) do
    {:ok, subscribers |> HashDict.put(pid_key, func)}
  end
  def handle_event({:remove_sub, pid_key}, subscribers = %HashDict{}) do
    {:ok, subscribers |> HashDict.delete pid_key}
  end
end

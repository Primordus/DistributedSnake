defmodule Snake.Gossip do
  use GenServer
  alias Snake.PubSub

  # TODO make mechanism for detecting node removal better (maybe polling?)

  @server __MODULE__
  @first_node :"pi1@tielen.local"

  defmodule State, do: defstruct pubsub: :no_pid

  # API

  @doc """
  Starts the gossip server.
  """
  def start_link do
    args = :ok
    GenServer.start_link(__MODULE__, args, [name: @server])
  end

  @doc """
  Subscribes a certain process to this gossip server with a certain function.
  """
  def subscribe(sub_function) when is_function(sub_function, 1) do
    :ok = @server |> GenServer.call {:subscribe, self, sub_function}
  end

  @doc """
  Unsubscribes a certain process from this gossip server process.
  """
  def unsubscribe do
    :ok = @server |> GenServer.call {:unsubscribe, self}
  end

  # GenServer callbacks

  @doc false
  def init(:ok) do
    Process.flag(:trap_exit, true)

    result = Node.ping @first_node
    :ok = result |> process_ping # Crashes if ping fails.

    {:ok, pubsub} = PubSub.start_link
    {:ok, %State{pubsub: pubsub}}
  end

  @doc false
  def handle_call({:subscribe, pid, sub_function}, _from, 
                  state = %State{pubsub: pubsub}) do
    pubsub |> PubSub.add_sub pid, sub_function
    {:reply, :ok, state}
  end
  def handle_call({:unsubscribe, pid}, _from, state = %State{pubsub: pubsub}) do
    pubsub |> PubSub.remove_sub pid
    {:reply, :ok, state}
  end
  def handle_call(_request, _from, state) do
    reply = {:error, :not_supported}
    {:reply, reply, state}
  end

  @doc false
  def handle_cast(event = {:added_node, _node}, 
                  state = %State{pubsub: pubsub}) do
    pubsub |> PubSub.publish event
    {:noreply, state}
  end
  def handle_cast(event = {:removed_node, _node}, 
                  state = %State{pubsub: pubsub}) do
    pubsub |> PubSub.publish event
    {:noreply, state}
  end
  
  @doc false
  def terminate(_reason, _state) do
    notify_node_removed
    :ok
  end

  # Helper functions
  defp process_ping(:pong) do
    this_node = Node.self
    # Spawns process on the first node (knows all other nodes already),
    # notifies everybody except this node of the change in the cluster.
    # TODO improve this code later..
    Node.spawn @first_node, fn ->
      notify_node_added(this_node)
    end

    :ok
  end
  defp process_ping(:pang), do: :not_ok

  defp notify_node_added(new_node) do
    # Sends an async message to all nodes (except new_node) that a new node
    # is added
    [Node.self | Node.list] 
      |> List.delete(new_node)
      |> GenServer.abcast @server, {:added_node, new_node}
  end

  # TODO improve this function
  defp notify_node_removed() do
    Node.list |> GenServer.abcast @server, {:removed_node, node()}
  end
end

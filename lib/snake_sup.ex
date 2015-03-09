defmodule Snake.SnakeSupervisor do
  use Supervisor
  alias Snake.Snake

  @moduledoc """
  Supervisor for all snake processes.
  """

  @sup __MODULE__

  @doc """
  Starts the supervisor.
  """
  def start_link(:ok), do: Supervisor.start_link(__MODULE__, :ok, [name: @sup])

  @doc false
  def init(:ok) do
    tree = [worker(Snake, [])]
    supervise(tree, strategy: :simple_one_for_one)
  end

  @doc """
  Starts a segment at new node in a certain state.
  """
  def start_child(new_node, state) do
    {@sup, new_node} |> Supervisor.start_child [state]
  end
end

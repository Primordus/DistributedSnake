defmodule Snake.InsectSupervisor do
  use Supervisor
  alias Snake.Insect

  @moduledoc """
  Supervisor for the insect process.
  """

  @sup __MODULE__

  @doc """
  Starts the supervisor.
  """
  def start_link(:ok), do: Supervisor.start_link(__MODULE__, :ok, [name: @sup])

  @doc false
  def init(:ok) do
    tree = [worker(Insect, [])]
    supervise(tree, strategy: :simple_one_for_one)
  end

  @doc """
  Starts a new insect process in the supervision tree.
  """
  def start_child(params = %{x: _x, y: _y, node: a_node}) do
    {@sup, a_node} |> Supervisor.start_child [params]
  end
end

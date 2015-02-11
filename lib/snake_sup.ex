defmodule Snake.SnakeSupervisor do
  use Supervisor

  def start_link(:ok), do: Supervisor.start_link(__MODULE__, :ok)

  def init(:ok) do
    tree = [worker(:snake, [])]
    supervise(tree, strategy: :simple_one_for_one)
  end
end

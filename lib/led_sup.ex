defmodule Snake.LedSupervisor do
  use Supervisor
  alias Snake.LED

  def start_link(:ok), do: Supervisor.start_link(__MODULE__, :ok)

  def init(:ok) do
    tree = [worker(LED, [])]
    supervise(tree, strategy: :simple_one_for_one)
  end
end

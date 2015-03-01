defmodule Snake.InsectSupervisor do
  use Supervisor
  alias Snake.Insect

  def start_link(:ok), do: Supervisor.start_link(__MODULE__, :ok)

  def init(:ok) do
    tree = [worker(Insect, [])]
    supervise(tree, strategy: :simple_one_for_one)
  end
end

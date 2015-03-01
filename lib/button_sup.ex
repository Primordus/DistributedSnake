defmodule Snake.ButtonSupervisor do
  use Supervisor
  alias Snake.Button

  def start_link(:ok), do: Supervisor.start_link(__MODULE__, :ok)

  def init(:ok) do
    tree = [worker(Button, [])]
    supervise(tree, strategy: :simple_one_for_one)
  end
end

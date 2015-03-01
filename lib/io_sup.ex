defmodule Snake.IO_Supervisor do
  use Supervisor
  alias Snake.Input
  alias Snake.LedSupervisor
  alias Snake.ButtonSupervisor

  def start_link(:ok), do: Supervisor.start_link(__MODULE__, :ok)

  def init(:ok) do
    tree = [worker(Input, []),
            supervisor(LedSupervisor, [:ok]),
            supervisor(ButtonSupervisor, [:ok])]
    supervise(tree, strategy: :one_for_all)
  end
end

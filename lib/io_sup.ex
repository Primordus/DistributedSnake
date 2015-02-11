defmodule Snake.IO_Supervisor do
  use Supervisor

  def start_link(:ok), do: Supervisor.start_link(__MODULE__, :ok)

  def init(:ok) do
    tree = [worker(:input_server, []),
            supervisor(Snake.LedSupervisor, [:ok]),
            supervisor(Snake.ButtonSupervisor, [:ok])]
    supervise(tree, strategy: :one_for_all)
  end
end

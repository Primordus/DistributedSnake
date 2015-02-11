defmodule Snake.TopSupervisor do
  use Supervisor

  def start_link, do: Supervisor.start_link(__MODULE__, :ok)

  def init(:ok) do
    tree = [worker(:gossip_server, []),
            supervisor(Snake.GameSupervisor, [:ok]),
            supervisor(Snake.IO_Supervisor, [:ok])]
    supervise(tree, strategy: :one_for_one)
  end
end

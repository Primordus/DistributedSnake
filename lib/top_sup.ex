defmodule Snake.TopSupervisor do
  use Supervisor
  alias Snake.Gossip
  alias Snake.GameSupervisor
  alias Snake.IO_Supervisor

  def start_link, do: Supervisor.start_link(__MODULE__, :ok)

  def init(:ok) do
    tree = [worker(Gossip, []),
            supervisor(GameSupervisor, [:ok]),
            supervisor(IO_Supervisor, [:ok])]
    supervise(tree, strategy: :one_for_one)
  end
end

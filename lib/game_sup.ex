defmodule Snake.GameSupervisor do
  use Supervisor

  def start_link(:ok), do: Supervisor.start_link(__MODULE__, :ok)

  def init(:ok) do
    tree = [supervisor(Snake.SnakeSupervisor, [:ok]),
            worker(:insect, []),
            worker(:board, []),
            worker(:game_tick, [])]
    supervise(tree, :one_for_all)
  end
end

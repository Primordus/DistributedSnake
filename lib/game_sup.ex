defmodule Snake.GameSupervisor do
  use Supervisor
  alias Snake.SnakeSupervisor
  alias Snake.InsectSupervisor
  alias Snake.BoardManager
  alias Snake.Board
  alias Snake.Ticker

  def start_link(:ok), do: Supervisor.start_link(__MODULE__, :ok)

  def init(:ok) do
    tree = [supervisor(SnakeSupervisor, [:ok]),
            supervisor(InsectSupervisor, [:ok]),
            worker(BoardManager, []),
            worker(Board, []),
            worker(Ticker, [])]
    supervise(tree, :one_for_all)
  end
end

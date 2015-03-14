defmodule Snake.GameSupervisor do
  use Supervisor
  alias Snake.TileSupervisor
  alias Snake.SnakeSupervisor
  alias Snake.InsectSupervisor
  alias Snake.BoardManager
  alias Snake.Board
  alias Snake.Ticker

  @moduledoc """
  Supervisor for the snake game.
  """

  @doc """
  Starts the game supervisor.
  """
  def start_link(:ok), do: Supervisor.start_link(__MODULE__, :ok)

  @doc false
  def init(:ok) do
    tree = [supervisor(TileSupervisor, [:ok]),
            supervisor(SnakeSupervisor, [:ok]),
            supervisor(InsectSupervisor, [:ok]),
            worker(BoardManager, []),
            worker(Board, []),
            worker(Ticker, [])]
    supervise(tree, strategy: :one_for_all)
  end
end

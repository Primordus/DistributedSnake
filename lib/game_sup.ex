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
    supervise(supervision_tree, strategy: :one_for_all)
  end

  # Helper functions

  defp supervision_tree do
    basic_tree = [worker(Board, []),
                  supervisor(TileSupervisor, [:ok]),
                  supervisor(SnakeSupervisor, [:ok]),
                  supervisor(InsectSupervisor, [:ok])] 
    # Wait a bit so global names get time to spread across cluster:
    :timer.sleep 300
    basic_tree
    |> check_board_mgr_alive
    |> check_ticker_alive
  end

  defp check_board_mgr_alive(tree) do
    BoardManager
    |> :global.whereis_name
    |> handle_board_mgr_alive(tree)
  end
  
  defp handle_board_mgr_alive(:undefined, tree) do
    [worker(BoardManager, []) | tree]
  end
  defp handle_board_mgr_alive(_, tree), do: tree

  defp check_ticker_alive(tree) do
    Ticker
    |> :global.whereis_name
    |> handle_ticker_alive(tree)
  end

  defp handle_ticker_alive(:undefined, tree) do
     [worker(Ticker, []) | tree]
  end
  defp handle_ticker_alive(_, tree), do: tree
end

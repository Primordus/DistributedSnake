defmodule Snake.TopSupervisor do
  use Supervisor
  alias Snake.Gossip
  alias Snake.GameSupervisor

  @moduledoc """
  Top supervisor for the snake application.
  """

  @doc """
  Starts the top supervisor.
  """
  def start_link, do: Supervisor.start_link(__MODULE__, :ok)

  @doc false
  def init(:ok) do
    tree = [worker(Gossip, []),
            supervisor(GameSupervisor, [:ok])]
    supervise(tree, strategy: :one_for_one)
  end
end

defmodule Snake.IO_Supervisor do
  use Supervisor
  alias Snake.Input

  @moduledoc """
  Supervisor for the game input (joystick / keyboard).
  """

  @doc """
  Starts the IO supervisor.
  """
  def start_link(:ok), do: Supervisor.start_link(__MODULE__, :ok)

  @doc false
  def init(:ok) do
    tree = [
      #worker(Input, [])
    ]
    supervise(tree, strategy: :one_for_all)
  end
end

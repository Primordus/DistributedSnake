defmodule Snake.ButtonSupervisor do
  use Supervisor
  alias Snake.Button

  @moduledoc """
  Supervisor for all button processes.
  """

  @sup __MODULE__

  @doc """
  Starts the supervisor.
  """
  def start_link(:ok), do: Supervisor.start_link(__MODULE__, :ok, [name: @sup])

  @doc false
  def init(:ok) do
    tree = [worker(Button, [])]
    supervise(tree, strategy: :simple_one_for_one)
  end

  @doc """
  Starts a new button process in the supervision tree.
  """
  def start_child(pin) when is_integer(pin) and pin > 0 do
    @sup |> Supervisor.start_child [pin]
  end
end

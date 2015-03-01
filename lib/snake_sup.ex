defmodule Snake.SnakeSupervisor do
  use Supervisor
  alias Snake.Snake

  def start_link(:ok), do: Supervisor.start_link(__MODULE__, :ok)

  def init(:ok) do
    tree = [worker(Snake, [])]
    supervise(tree, strategy: :simple_one_for_one)
  end

  @doc """
  Starts a segment at new node in a certain state.
  """
  def start_child(NewNode, State) do
    Supervisor.start_child({__MODULE__, NewNode}, [State])
  end
end

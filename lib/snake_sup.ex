defmodule Snake.SnakeSupervisor do
  use Supervisor
  alias Snake.Snake

  @moduledoc """
  Supervisor for all snake processes.
  """

  @sup __MODULE__

  @doc """
  Starts the supervisor.
  """
  def start_link(:ok) do 
    {:ok, sup} = Supervisor.start_link(__MODULE__, :ok, [name: @sup])
    {:ok, _snake} =
      Snake
      |> :global.whereis_name
      |> handle_snake_alive
    {:ok, sup}
  end

  @doc false
  def init(:ok) do
    tree = [worker(Snake, [])]
    supervise(tree, strategy: :simple_one_for_one)
  end

  @doc """
  Starts a segment at new node in a certain state.
  """
  def start_child(new_node) do
    {@sup, new_node} |> Supervisor.start_child []
  end

  # Helper functions
  defp handle_snake_alive(:undefined), do: spawn_snake
  defp handle_snake_alive(snake_pid),  do: {:ok, snake_pid}

  defp spawn_snake, do: Node.self |> start_child 
end

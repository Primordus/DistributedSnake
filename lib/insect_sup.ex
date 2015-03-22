defmodule Snake.InsectSupervisor do
  use Supervisor
  use Snake.Game
  alias Snake.Random
  alias Snake.Insect
  alias GPIO.LedSupervisor, as: LED_Sup

  @moduledoc """
  Supervisor for the insect process.
  """

  @sup __MODULE__

  @doc """
  Starts the supervisor.
  """
  def start_link(:ok) do
    {:ok, sup} = Supervisor.start_link(__MODULE__, :ok, [name: @sup])
    {:ok, _insect} = 
      Insect
      |> :global.whereis_name
      |> handle_insect_alive
    {:ok, sup}
  end

  @doc false
  def init(:ok) do
    pin = 18
    {:ok, led} = spawn_led(pin)
    tree = [worker(Insect, [led])]
    supervise(tree, strategy: :simple_one_for_one)
  end

  @doc """
  Starts a new insect process in the supervision tree on a specific node.
  """
  def start_child(%{x: x, y: y, node: a_node}) do
    {@sup, a_node} |> Supervisor.start_child [%{x: x, y: y}]
  end

  defp handle_insect_alive(:undefined), do: spawn_insect
  defp handle_insect_alive(insect_pid), do: {:ok, insect_pid}

  defp spawn_insect do
    Random.generate_seed
    random_x = Random.number(width - 1)
    random_y = Random.number(height - 1)
    random_node = [Node.self | Node.list] |> Random.element
    {:ok, _} = start_child %{x: random_x, y: random_y, node: random_node}
  end

  


  # TODO remove following code later..

  defp spawn_led(pin), do: do_spawn(Mix.env, pin) # TODO remove GPIO code later

  defp do_spawn(:test, _pin) do
    pid = spawn_link fn ->
      receive do
        _ -> :ok
      end
    end

    {:ok, pid}
  end
  defp do_spawn(_env, pin), do: do_spawn(:test, pin)
  #defp do_spawn(_env, pin), do: LED_Sup.start_child(pin)
end

defmodule SnakeTest do
  use ExUnit.Case, async: true
  alias Snake.SnakeSupervisor
  alias Snake.InsectSupervisor
  alias Snake.Snake.State, as: SnakeState
  alias Snake.Insect.State, as: InsectState

  @moduledoc """
  Tests for application and supervisor tree.
  """

  test "Application starts correctly" do
    assert Application.start(:snake) == {:error, {:already_started, :snake}}
  end

  #test "Spawning snake process in supervision tree" do
  #  assert count_children(SnakeSupervisor) == 0
  #  
  #  SnakeSupervisor.start_child node, %SnakeState{}
  #  assert count_children(SnakeSupervisor) == 1
  #
  # SnakeSupervisor.start_child node, %SnakeState{}
  # assert count_children(SnakeSupervisor) == 2
  #end
  #
  #test "Spawning insect process in supervision tree" do
  #  assert count_children(InsectSupervisor) == 0
  #  
  # InsectSupervisor.start_child node, %{x: 0, y: 1, node: Node.self}
  # assert count_children(InsectSupervisor) == 1
  #
  # InsectSupervisor.start_child node, %{x: 0, y: 1, node: Node.self}
  #  assert count_children(InsectSupervisor) == 2
  #end
 
  # LED and button should only be tested on Raspberry Pi 2.
  # Tests similar to previous 2

  defp count_children(sup) do
    %{workers: amount} = sup |> Supervisor.count_children
    amount
  end

  defp sleep(ms), do: :timer.sleep(ms)
end

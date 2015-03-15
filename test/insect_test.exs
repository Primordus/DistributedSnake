defmodule Snake.InsectTest do
  use ExUnit.Case
  use Snake.Game
  alias Snake.Insect
  alias Snake.InsectSupervisor, as: InsectSup

  @moduledoc """
  Tests for the Insect / InsectSupervisor modules.
  """

  setup_all do 
    Application.ensure_all_started :snake
    sleep 30  # Give insect time to start properly.
  end

  test "InsectSupervisor is a locally registered process" do
    refute Process.whereis(InsectSup) == nil
  end

  test "Insect is a globally registered process" do
    assert Insect in :global.registered_names
  end

  test "Insect can return the current location" do
    {x, y, a_node} = Insect.get_location
    assert x in 0..width
    assert y in 0..height
    assert a_node in [Node.self | Node.list]
  end

  test "Insect starts new insect after being killed" do
    insect = :global.whereis_name(Insect)
    insect_amount = count_children(InsectSup)
    assert is_pid(insect)
    Insect.kill
    sleep 40
    refute :global.whereis_name(Insect) == insect
    assert insect_amount == count_children(InsectSup)
    sleep 10
    # TODO check supervision tree for children also!
  end

  test "Insect changes location/node when countdown reaches 0" do
    {x1, y1, node1} = Insect.get_location
    simulate_update_state
    {x2, y2, node2} = Insect.get_location
    refute x1 == x2 and y1 == y2 and node1 == node2
  end

  defp sleep(ms), do: :timer.sleep(ms)

  defp count_children(sup) do
    sup
    |> Supervisor.which_children
    |> length
  end

  defp simulate_update_state do
    0..10 |> Enum.map fn(_) ->
      {:global, Insect} |> GenServer.call :update_state
    end
    sleep 40
  end
end

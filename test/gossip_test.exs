defmodule Snake.GossipTest do
  use ExUnit.Case, async: true
  alias Snake.Gossip

  @moduledoc """
  Tests for the gossip module.
  """

  setup_all do
    # Test registering of process:
    assert Process.whereis(Gossip) == nil
    {:ok, _pid} = Gossip.start_link
    refute Process.whereis(Gossip) == nil

    :ok
  end

  test "Subscribing and unsubscribing to gossip process" do
    pid = self
    delay = 10

    simulate_node_added :dummy_node1
    refute_receive :dummy_node1, delay

    Gossip.subscribe self, fn(event) ->
      pid |> send event
    end

    simulate_node_added :dummy_node2
    assert_receive {:added_node, :dummy_node2}

    simulate_node_removed :dummy_node3
    assert_receive {:removed_node, :dummy_node3}

    Gossip.unsubscribe self
    simulate_node_added :dummy_node4
    refute_receive {:added_node, :dummy_node4}, delay

    simulate_node_removed :dummy_node5
    refute_receive {:removed_node, :dummy_node5}, delay
  end

  # Next test was tested manually.
  # test "Node addition and removal" do
  #   TODO figure out how to write this in a unit test!
  # end

  # Helper functions 

  defp simulate_node_added(node_name) do
    Gossip |> GenServer.cast {:added_node, node_name}
  end

  defp simulate_node_removed(node_name) do
    Gossip |> GenServer.cast {:removed_node, node_name}
  end
end

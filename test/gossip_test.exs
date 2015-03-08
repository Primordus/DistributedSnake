defmodule Snake.GossipTest do
  use ExUnit.Case, async: true
  alias Snake.Gossip

  @moduledoc """
  Tests for the gossip module.
  """

  test "Gossip is a registered process" do
    refute Process.whereis(Gossip) == nil
  end

  test "Subscribing and unsubscribing to gossip process" do
    pid = self
    delay = 10
    a_node = Node.self

    simulate_node_added a_node
    refute_receive {:added_node, ^a_node}, delay
    simulate_node_removed a_node
    refute_receive {:removed_node, ^a_node}, delay

    Gossip.subscribe self, fn(event) ->
      pid |> send event
    end

    simulate_node_added(a_node)
    assert_receive {:added_node, ^a_node}
    simulate_node_removed(a_node)
    assert_receive {:removed_node, ^a_node}

    Gossip.unsubscribe self
    
    simulate_node_added a_node
    refute_receive {:added_node, ^a_node}, delay
    simulate_node_removed a_node
    refute_receive {:removed_node, ^a_node}, delay
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

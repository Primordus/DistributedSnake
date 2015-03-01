defmodule Snake.PubSubTest do
  use ExUnit.Case, async: true
  alias Snake.PubSub

  @moduledoc """
  Tests for the PubSub process.
  """

  test "starting and stopping of pubsub process" do
    # Start pubsub process.
    {:ok, pubsub} = PubSub.start_link
    assert Process.alive?(pubsub)

    # Stop the process => should not be alive anymore.
    pubsub |> PubSub.stop
    refute Process.alive?(pubsub)
    # stop not explicitly needed in another processes because of link
  end

  test "publishing different kinds of events" do
    first_msg = :dummy_data
    second_msg = "blabla"
    third_msg = 123456789
    
    {:ok, pubsub} = PubSub.start_link
    
    pubsub |> PubSub.publish first_msg
    refute_receive ^first_msg, 10
  
    # Send messages from pubsub back to ourself to test result.
    pubsub |> PubSub.add_sub self
    
    pubsub |> PubSub.publish second_msg
    pubsub |> PubSub.publish third_msg

    assert_receive ^second_msg
    assert_receive ^third_msg
  end

  test "adding and removing of subscribers" do
    # Tested by forwarding messages back to the test process.
    msg = "test123"
  
    # Support for 1 subscriber:
    {:ok, pubsub} = PubSub.start_link
    pubsub |> PubSub.publish msg
    refute_receive ^msg, 10

    pubsub |> PubSub.add_sub self
    pubsub |> PubSub.publish msg
    assert_receive ^msg
    
    # Spawn dummy process to check if key works properly.
    pubsub |> PubSub.remove_sub(spawn_dummy)
    pubsub |> PubSub.publish msg
    assert_receive ^msg

    # Now properly remove subscriber
    pubsub |> PubSub.remove_sub self
    pubsub |> PubSub.publish msg
    refute_receive ^msg, 10

    # Support for multiple subscribers 
    pid = self
    pid2 = spawn_link fn -> # Spawn 2nd process
      dummy_loop(pubsub, pid) 
    end
    pubsub |> PubSub.add_sub self
    pubsub |> PubSub.add_sub pid2
    pubsub |> PubSub.publish msg
    assert_receive ^msg
    assert_receive ^msg

    pubsub |> PubSub.remove_sub self
    pubsub |> PubSub.publish msg
    assert_receive ^msg
    refute_receive ^msg, 10
  end

  defp spawn_dummy do
    spawn_link fn ->
      receive do
        _ -> :ok
      end
    end
  end

  defp dummy_loop(pubsub, pid) do 
    receive do # simply forward messages
      msg ->
        pid |> send msg
    end

    dummy_loop(pubsub, pid)
  end
end

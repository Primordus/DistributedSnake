defmodule Snake.TickerTest do
  use ExUnit.Case, async: true
  alias Snake.Ticker

  @moduledoc """
  Tests for the ticker module.
  """

  test "Starting of ticker process (0 args)." do
    {:ok, _ticker} = Ticker.start_link
  end

  test "Starting of ticker process (1 arg)" do
    assert_raise FunctionClauseError, fn -> Ticker.start_link -1 end
    assert_raise FunctionClauseError, fn -> Ticker.start_link 0 end
    {:ok, _pid} = Ticker.start_link(5000)
  end

  test "(Globally) registered ticker process." do
    {:ok, _pid} = Ticker.start_link
    # Globally registered process, does not show up in Process.registered()!
    assert Ticker in :global.registered_names
    {:error, {:already_started, ^_pid}} = Ticker.start_link
  end
  
  test "Different delays of ticks" do
    expected_msg1 = {:test_data, :tick}
    expected_msg2 = {"test", :tick}
    first_delay = 10
    second_delay = 3
    pid = self

    {:ok, _pid1} = Ticker.start_link(first_delay)
    refute_receive ^expected_msg1, first_delay
    
    Ticker.subscribe self, fn(msg) ->
      pid |> send {:test_data, msg}
    end

    # Give a bit more delay just to be safe.
    assert_receive ^expected_msg1, 2 * first_delay
    assert_receive ^expected_msg1, 2 * first_delay
    
    # Manually unregister the process to enable multiple tickers in this test.
    :global.unregister_name(Ticker)

    {:ok, _pid2} = Ticker.start_link(second_delay)
    Ticker.subscribe self, fn(msg) ->
      pid |> send {"test", msg}
    end

    # Give a bit more delay just to be safe.
    assert_receive ^expected_msg2, 2 * second_delay
    assert_receive ^expected_msg2, 2 * second_delay
  end

  test "Subscribing and unsubscribing from/to ticker process" do
    pid = self
    delay = 5

    {:ok, _ticker} = Ticker.start_link delay
    Ticker.subscribe self, fn(msg) ->
      pid |> send msg
    end

    # Give a bit more delay just to be safe.
    assert_receive :tick, 2 * delay
    assert_receive :tick, 2 * delay

    Ticker.unsubscribe self
    refute_receive :tick, 2 * delay
  end
end

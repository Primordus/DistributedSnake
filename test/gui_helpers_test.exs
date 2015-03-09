defmodule Snake.GuiTest do
  use ExUnit.Case, async: true
  alias Snake.GUI
  alias Phoenix.Socket.Message

  @delay 10

  test "PubSub for draw_snake" do
    draw_msg = %{x: 0, y: 0, node: Node.self, color: :yellow}
    expected_result = {:socket_broadcast, %Message{topic: "snake", 
                                                    event: "draw_snake",
                                                    payload: draw_msg}}
    GUI.draw_snake draw_msg
    refute_receive ^expected_result, @delay

    Phoenix.PubSub.subscribe self, "snake"
    GUI.draw_snake draw_msg
    assert_receive ^expected_result, @delay
    
    Phoenix.PubSub.unsubscribe self, "snake"
    GUI.draw_snake draw_msg
    refute_receive ^expected_result, @delay
  end

  test "draw_snake guards" do
    e = FunctionClauseError
    bad_values_xy = ["boo", -1, 0.1]
    bad_values_color = [1, "bad", []]

    # Correct usage

    GUI.draw_snake %{x: 0, y: 3, node: Node.self, color: :white}

    # Bad usages

    # Check x and y guard
    bad_values_xy |> Enum.map fn(bad_value) ->
      assert_raise e, fn -> 
        GUI.draw_snake %{x: bad_value, y: 3, node: Node.self, color: :white} 
      end
      assert_raise e, fn ->
        GUI.draw_snake %{x: 0, y: bad_value, node: Node.self, color: :white}
      end
    end

    # Check color guard
    bad_values_color |> Enum.map fn(bad_value) ->
      assert_raise e, fn ->
        GUI.draw_snake %{x: 0, y: 3, node: Node.self, color: bad_value}
      end
    end
  end

  test "PubSub for draw_insect" do
    draw_msg = %{x: 0, y: 0, node: Node.self}
    expected_result = {:socket_broadcast, %Message{topic: "snake", 
                                                    event: "draw_insect",
                                                    payload: draw_msg}}
    GUI.draw_insect draw_msg
    refute_receive ^expected_result, @delay

    Phoenix.PubSub.subscribe self, "snake"
    GUI.draw_insect draw_msg
    assert_receive ^expected_result, @delay
    
    Phoenix.PubSub.unsubscribe self, "snake"
    GUI.draw_insect draw_msg
    refute_receive ^expected_result, @delay
  end

  test "draw_insect guards" do
    e = FunctionClauseError
    bad_values_xy = ["boo", -1, 0.1, {}, []]

    # Correct usage

    GUI.draw_insect %{x: 0, y: 3, node: Node.self}

    # Bad usages

    # Check x and y guard
    bad_values_xy |> Enum.map fn(bad_value) ->
      assert_raise e, fn -> 
        GUI.draw_insect %{x: bad_value, y: 3, node: Node.self}
      end
      assert_raise e, fn ->
        GUI.draw_insect %{x: 0, y: bad_value, node: Node.self}
      end
    end
  end

  test "PubSub for update_score" do
    score_msg = %{score: 1000}
    expected_result = {:socket_broadcast, %Message{topic: "snake", 
                                                    event: "score",
                                                    payload: score_msg}}
    GUI.update_score score_msg
    refute_receive ^expected_result, @delay

    Phoenix.PubSub.subscribe self, "snake"
    GUI.update_score score_msg
    assert_receive ^expected_result, @delay
    
    Phoenix.PubSub.unsubscribe self, "snake"
    GUI.update_score score_msg
    refute_receive ^expected_result, @delay
  end

  test "update_score guards" do
    e = FunctionClauseError
    bad_values = [:five, "five", {}, []]

    bad_values |> Enum.map fn(bad_value) ->
      assert_raise e, fn ->
        GUI.update_score bad_value
      end
    end
  end

  test "PubSub for clear_tile" do
    clear_msg = %{x: 1, y: 2, node: Node.self}
    expected_result = {:socket_broadcast, %Message{topic: "snake", 
                                                    event: "clear_tile",
                                                    payload: clear_msg}}
    GUI.clear_tile clear_msg
    refute_receive ^expected_result, @delay

    Phoenix.PubSub.subscribe self, "snake"
    GUI.clear_tile clear_msg
    assert_receive ^expected_result, @delay
    
    Phoenix.PubSub.unsubscribe self, "snake"
    GUI.clear_tile clear_msg
    refute_receive ^expected_result, @delay   
  end

  test "clear_tile guards" do
    e = FunctionClauseError
    bad_values_xy = ["boo", -1, 0.1, {}, []]

    # Correct usage

    GUI.clear_tile %{x: 0, y: 3, node: Node.self}

    # Bad usages

    # Check x and y guard
    bad_values_xy |> Enum.map fn(bad_value) ->
      assert_raise e, fn -> 
        GUI.clear_tile %{x: bad_value, y: 3, node: Node.self}
      end
      assert_raise e, fn ->
        GUI.clear_tile %{x: 0, y: bad_value, node: Node.self}
      end
    end
  end
end

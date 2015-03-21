defmodule Snake.SnakeChannel do
  use Phoenix.Channel
  alias Snake.Insect
  alias Snake.Snake

  @moduledoc """
  Websockets logic!
  Main part is in javascript, this is mainly to enable them + logic that
  tells the game to send which drawing commands to which GUI.
  """

  def join("snake", _auth_msg, socket) do
    {:ok, socket} # Simply allow all users to connect and view the game.
  end

  def handle_in("draw_all", _msg, socket) do
    Snake.draw
    Insect.draw
    {:ok, socket}
  end
  def handle_in("new_input", %{"input" => "space"}, socket) do
    Snake.subscribe_to_ticker
    Insect.subscribe_to_ticker
    {:ok, socket}
  end
  def handle_in("new_input", %{"input" => direction}, socket) do
    direction |> Snake.set_direction
    {:ok, socket}
  end
  def handle_in(_topic, _msg, socket) do
    # Catch all remaining clauses to handle all msges without processing
    {:ok, socket}
  end

  def handle_out(topic = "draw_snake", draw_snake_msg, socket) do
    handle_draw_snake(socket, topic, draw_snake_msg, Node.self)
  end

  def handle_out(topic = "draw_insect", draw_insect_msg, socket) do
    handle_draw_insect(socket, topic, draw_insect_msg, Node.self)
  end
  
  def handle_out(topic = "clear_tile", clear_tile_msg, socket) do
    handle_clear_tile(socket, topic, clear_tile_msg, Node.self)
  end

  def handle_out(topic = "score", score, socket) do 
    # Forward score to all GUIs!
    reply socket, topic, score
  end
  def handle_out(topic = "reset_score", msg, socket) do
    reply socket, topic, msg
  end

  # Catch all clause to forward all other msges without processing
  def handle_out(topic, msg, socket) do
    reply(socket, topic, msg)
  end

  # Helper functions

  defp handle_draw_snake(socket, topic,
                        msg = %{x: _x, y: _y, node: this_node, color: _color}, 
                        this_node) do
    reply socket, topic, msg
  end
  defp handle_draw_snake(socket, _topic, _msg, _this_node) do
    # Drop the message, is intended for another node!
    {:ok, socket}
  end

  defp handle_draw_insect(socket, topic,
                          msg = %{x: _x, y: _y, node: this_node}, 
                          this_node) do
    reply socket, topic, msg
  end
  defp handle_draw_insect(socket, _topic, _msg, _this_node) do
    {:ok, socket}
  end

  defp handle_clear_tile(socket, topic, 
                          msg = %{x: _x, y: _y, node: this_node}, 
                          this_node) do
    reply socket, topic, msg
  end
  defp handle_clear_tile(socket, _topic, _msg, _this_node) do
    {:ok, socket}
  end
end

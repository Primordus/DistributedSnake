defmodule Snake.GUI do
  
  @moduledoc """
  Module containing some helper functions to easily draw on the canvas 
  in the GUI.
  """

  # TODO unit test the broadcast system (if properly connected)?

  @insect_color :red
  
  # API

  @doc """
  Draws a segment of the snake at position {x, y} on a specific node 
  with a certain color.
  """
  def draw_snake(msg = %{x: x, y: y, node: _node, color: color})
      when is_integer(x) and x > -1 
      and is_integer(y) and y > -1
      and is_atom(color) do
    notify_gui "draw_snake", msg
  end

  @doc """
  Draws a insect at location {x, y} on a specific node.
  """
  def draw_insect(msg = %{x: x, y: y, node: _node}) 
      when is_integer(x) and x > -1 
      and is_integer(y) and y > -1 do
    notify_gui "draw_insect", msg
  end

  @doc """
  Updates the score on the GUI to the new score.
  """
  def update_score(msg = %{score: score}) when is_number(score) do
    notify_gui "score", msg
  end

  def clear_tile(msg = %{x: x, y: y, node: _node}) 
      when is_integer(x) and x > -1
      and is_integer(y) and y > -1 do
    notify_gui "clear_tile", msg
  end

  defp notify_gui(topic, msg) do
    Phoenix.Channel.broadcast("snake", topic, msg)
  end
end

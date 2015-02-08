defmodule Snake.SnakeChannel do
  use Phoenix.Channel

  # Websockets logic!
  # (main part is in javascript, this is mainly to enable them)

  def join("snake", _auth_msg, socket) do
    {:ok, socket} # Simply allow all users to connect and view the game.
  end

  # Catch all clause to handle all msges without processing
  def handle_in(_topic, _msg, socket), do: {:ok, socket}

  # handle_in and handle_out not needed in this case,
  def handle_out(topic = "score", msg, socket) do 
    # TODO make other subtopics later! and check board params
    reply socket, topic, msg
  end

  # Catch all clause to forward all other msges without processing
  def handle_out(topic, msg, socket), do: reply(socket, topic, msg)
end

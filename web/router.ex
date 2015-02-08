defmodule Snake.Router do
  use Phoenix.Router

  pipeline :browser do
    plug :accepts, ~w(html)
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
  end

  pipeline :api do
    plug :accepts, ~w(json)
  end

  scope "/", Snake do
    pipe_through :browser # Use the default browser stack
    get "/", SnakeController, :index
  end

  socket "/ws", Snake do
    channel "snake", SnakeChannel
  end
end

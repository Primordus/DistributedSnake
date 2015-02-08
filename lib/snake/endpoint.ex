defmodule Snake.Endpoint do
  use Phoenix.Endpoint, otp_app: :snake

  plug Plug.Static, at: "/", from: :snake
  plug Plug.Logger

  # Code reloading will only work if the :code_reloader key of
  # the :phoenix application is set to true in your config file.
  plug Phoenix.CodeReloader
  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Poison

  plug Plug.MethodOverride
  plug Plug.Head
  plug Plug.Session,
    store: :cookie,
    key: "_snake_key",
    signing_salt: "9BgUHEr6",
    encryption_salt: "gEVUjiGY"
  plug :router, Snake.Router
end

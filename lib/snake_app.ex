defmodule Snake do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      # Top supervisor for the snake game:
      supervisor(Snake.TopSupervisor, []),
      # Start the endpoint when the application starts:
      worker(Snake.Endpoint, [])
    ]

    opts = [strategy: :one_for_one, name: Snake.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    Snake.Endpoint.config_change(changed, removed)
    :ok
  end
end

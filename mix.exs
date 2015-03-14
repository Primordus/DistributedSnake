defmodule Snake.Mixfile do
  use Mix.Project

  def project do
    [app: :snake,
     version: "0.0.1",
     elixir: "~> 1.0",
     elixirc_paths: ["lib", "web"],
     compilers: [:phoenix] ++ Mix.compilers,
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [mod: {Snake, []},
      applications: [:phoenix,
                      :cowboy,
                      :logger,
                      :gpio_rpi,
                      :crypto],
      registered: [Snake.Board, 
                    Snake.BoardManager,
                    Snake.Gossip,
                    Snake.Insect,
                    Snake.InsectSupervisor,
                    Snake.Snake,
                    Snake.SnakeSupervisor,
                    Snake.Ticker,
                    Snake.TileSupervisor]]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type `mix help deps` for more examples and options
  defp deps do
     [{:phoenix, "~> 0.8.0"},
     {:cowboy, "~> 1.0"},
      {:gpio_rpi, git: "git://github.com/Primordus/gpio_rpi.git"}]
  end
end

defmodule Snake.Game do
  @moduledoc """
  Small mixin containing some helper macros for the game model.
  """

  defmacro __using__(_opts) do
    quote do
      import Snake.Game
      defmacro width, do: 10
      defmacro height, do: 10
    end
  end
end

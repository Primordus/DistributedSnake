defmodule Snake.Random do

  @moduledoc """
  Module with helper functions to generate random numbers, pick random 
  elements out of a list, ...
  """
  
  @doc """
  Generates a new seed. Best used once before using the other functions to
  make sure that they are random.
  """
  def generate_seed do
    <<a :: 32, b :: 32, c :: 32>> = :crypto.rand_bytes(12)
    :random.seed {a, b, c}
  end

  @doc """
  Generates a random number between 0 and max.
  """
  def number(max), do: floor(:random.uniform * max)

  @doc """
  Picks a random element out of the list.
  """
  def element(list), do: list |> element(length list)
  
  defp element(list, length) do
    random_index = floor(:random.uniform * length)
    list |> Enum.at random_index
  end

  defp floor(number), do: number |> Float.floor |> round
end

defmodule Snake.Button do
  use GenServer
  alias Snake.GPIO

  @moduledoc """
  Module that enables the user to read the value of a GPIO input pin
  (for example button state).
  
  Test from command prompt:
  
  > sudo iex -S mix
  > {:ok, button} = Snake.Button.start_link(18)
  > value = Snake.Button.get_value(button)
    #=> :high  # or :low
  > Snake.button.stop(button)
  """

  @server __MODULE__

  defmodule State do
    defstruct pin: :no_pin
  end

  # API

  @doc """
  Starts a button process.
  """
  def start_link(pin) when pin > 0 do
    args = pin
    GenServer.start_link(@server, args)
  end

  @doc """
  Read the state of a button (returns :high or :low).
  """
  def get_value(button) when is_pid(button) do
    button |> GenServer.call :read
  end

  @doc """
  Stops a button process.
  """
  def stop(button) when is_pid(button) do
    :ok = button |> GenServer.call :stop
  end

  # GenServer callbacks

  @doc false
  def init(pin) do
    Process.flag(:trap_exit, true)
    pin |> GPIO.pin_mode :input
    {:ok, %State{pin: pin}}
  end

  @doc false
  def handle_call(:read, _from, state = %State{pin: pin}) do
    {:reply, pin |> GPIO.digital_read, state}
  end
  def handle_call(:stop, _from, state = %State{}) do
    reply = :ok
    {:stop, :normal, reply, state}
  end
  def handle_call(_request, _from, state) do
    {:reply, {:error, :not_supported}, state}
  end

  @doc false
  def terminate(_reason, %State{pin: pin}) do
    pin |> GPIO.pin_release
  end
end

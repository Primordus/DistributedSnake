defmodule Snake.LED do
  use GenServer
  alias Snake.GPIO
  
  @moduledoc """
  Module for controlling a LED with the Raspberry Pi GPIO's.

  Test from command prompt:

  > sudo iex -S mix
  > {:ok, led} = Snake.LED.start_link(18)
  > led |> Snake.LED.on
  > led |> Snake.LED.off
  > led |> Snake.LED.stop
  """

  @server __MODULE__

  defmodule State, do: defstruct pin: :no_pin

  # API

  @doc """
  Starts a LED process.
  """
  def start_link(pin) when Pin > 0, do: GenServer.start_link(@server, pin)

  @doc """
  Stops a LED process.
  """
  def stop(led), do: :ok = led |> GenServer.call :stop

  @doc """
  Turns the LED on.
  """
  def on(led) when is_pid(led), do: :ok = led |> GenServer.call :on

  @doc """
  Turns the LED off.
  """
  def off(led) when is_pid(led), do: :ok = led |> GenServer.call :off

  @doc """
  TODO
  """
  def pulse(led, time \\ 500) when is_pid(led) and time > 0 do 
    led |> GenServer.cast {:pulse, time}   
  end

  # GenServer callbacks

  @doc false
  def init(pin) do
    Process.flag(:trap_exit, true)
    pin |> GPIO.pin_mode :output
    {:ok, %State{pin: pin}}
  end

  @doc false
  def handle_call(:on, _from, state = %State{pin: pin}) do
    pin |> GPIO.digital_write :high
    {:reply, :ok, state}
  end
  def handle_call(:off, _from, state = %State{pin: pin}) do
    pin |> GPIO.digital_write :low
    {:reply, :ok, state}
  end
  def handle_call(:stop, _from, state = %State{}) do
    reply = :ok
    {:stop, :normal, reply, state}
  end
  def handle_call(_request, _from, state) do
    {:reply, {:error, :not_supported}, state}
  end

  @doc false
  def handle_cast({:pulse, time}, state = %State{pin: pin}) do
    pin |> GPIO.digital_write :high
    sleep time
    pin |> GPIO.digital_write :low
    {:noreply, state}
  end
  def handle_cast(_request, state = %State{}) do
    {:noreply, state}
  end

  @doc false
  def terminate(_reason, %State{pin: pin}), do: pin |> GPIO.pin_release

  # Helper functions

  defp sleep(time), do: :timer.sleep(time)
end

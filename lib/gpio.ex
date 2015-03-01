defmodule Snake.GPIO do

  @moduledoc """
  Module that exposes an Arduino-like API for controlling the GPIO pins
  of the Raspberry Pi.

  RaspBerry Pi 2: Hardware pins
  -----------------------------

  ______________________
  |                1  2 |
  |                3  4 |
  |                5  5 |
  |                7  8 |
  |               .. .. |
  |                     |
  |               39 40 |
  |                     |
  |                     |
  |                     |
  |                     |
  |  ___    ___   ___   |
  | |eth|  |USB| |USB|  |
  ______________________


  Pin 1  = 3.3V
  Pin 6  = Ground
  Pin 12 = I/O 18

  READING INPUT
  _____________

  Electric scheme:
  Pin 1 ___ R ____/ __ Pin 6
               |
             Pin 12


  Test from command-prompt:
  > sudo iex -S mix
  > Snake.GPIO.pin_mode(18, input).
  > Snake.GPIO.digital_read(18).


  WRITING OUTPUT
  ______________

  Electric scheme:
  Pin 12 ___ R __ LED __ Pin 6


  Test from command-prompt:
  > sudo iex -S mix
  > Snake.GPIO.pin_mode(18, output).
  > Snake.GPIO.digital_write(18, high).
  > Snake.GPIO.digital_write(18, low).

  """

  # API:

  @doc """
  Registers the pin as input or output.
  """
  def pin_mode(pin, :output) when pin > 0 do
    pin 
    |> export
    |> pin_direction_path
    |> write_to_file "out"

    pin |> digital_write :low # TODO pin goes high on output?? check electric circuit?
  end
  def pin_mode(pin, :input) when pin > 0 do
    pin 
    |> export
    |> pin_direction_path
    |> write_to_file "in"
  end

  @doc """
  Unregisters the pin as input or output.
  """
  def pin_release(pin) when pin > 0 do
    contents = Integer.to_string(pin)
    write_to_file("/sys/class/gpio/unexport", contents)
  end

  @doc """
  Writes a value to a certain pin.
  """
  def digital_write(pin, :high) when pin > 0 do
    pin |> pin_value_path |> write_to_file "1"
  end
  def digital_write(pin, :low) when pin > 0 do
    pin |> pin_value_path |> write_to_file "0"
  end

  @doc """
  Reads a value from a certain pin.
  """
  def digital_read(pin) when pin > 0 do
    pin 
    |> pin_value_path 
    |> read_from_file(1)
    |> handle_input_result
  end

  # Helper functions

  defp handle_input_result("0"), do: :low
  defp handle_input_result("1"), do: :high

  # Exports a pin as input or output.
  defp export(pin) do
    write_to_file "/sys/class/gpio/export", Integer.to_string(pin)
    pin
  end

  # Retrieves the path to the file that controls the GPIO pin direction.
  defp pin_direction_path(pin), do: pin_path(pin) <> "/direction"

  # Retrieves the path to the file to read/write a value from/to the GPIO pin.
  defp pin_value_path(pin), do: pin_path(pin) <> "/value"

  # Generates a path to a file based on pin number.
  defp pin_path(pin), do: "/sys/class/gpio/gpio" <> Integer.to_string(pin)

  defp write_to_file(file_path, contents) do
    File.open! file_path, [:write], fn(file) ->
      IO.binwrite file, contents  # binary write
    end
  end

  defp read_from_file(file_path, length) do
    File.open! file_path, [:read], fn(file) ->
      IO.binread(file, length)  # binary read
    end
  end
end

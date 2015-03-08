defmodule SnakeTest do
  use ExUnit.Case, async: true

  @moduledoc """
  Tests for application and supervisor tree.
  """

  test "Application starts correctly" do
    assert Application.start(:snake) == {:error, {:already_started, :snake}}
  end
end

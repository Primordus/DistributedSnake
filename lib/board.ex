defmodule Snake.Board do
  alias Snake.Board
  alias Snake.BoardManager

  @moduledoc """
  Module containing logic to link multiple playing boards together.
  """

  @server __MODULE__

  defmodule State do
    defstruct up: :no_board, down: :no_board,
              right: :no_board, left: :no_board
  end

  # Board API
  # Due to the nature of this process, all requests are handled synchronously.

  @doc """
  Starts a board process.
  """
  def start_link, do: GenServer.start_link(@server, :ok, [name: @server])

  @doc """
  Stops a board process.
  """
  def stop, do: :ok = @server |> GenServer.call :stop

  @doc """
  Returns a tuple with which you can address the board on this node easily.
  """
  def local_board, do: {@server, Node.self}

  @doc """
  Gives back a list of all boards ([{Board, Node.self}]).
  """
  def get_all, do: [Node.self | Node.list] |> Enum.map &({Board, &1})

  @doc """
  Gives back an adjacent board (or :no_board) of the board on this node.
  """
  def get(:left), do: local_board |> GenServer.call {:get, :left}
  def get(:right), do: local_board |> GenServer.call {:get, :right}
  def get(:up), do: local_board |> GenServer.call {:get, :up}
  def get(:down), do: local_board |> GenServer.call {:get, :down}

  @doc """
  Gives back an adjacent board (or :no_board) of the board.
  """
  def get(:left_of, board), do: board |> GenServer.call {:get, :left}
  def get(:right_of, board), do: board |> GenServer.call {:get, :right}
  def get(:up_of, board), do: board |> GenServer.call {:get, :up}
  def get(:down_of, board), do: board |> GenServer.call {:get, :down}

  @doc """
  Adds one board adjacent to another board. (boardX = {@server, node})
  Parameter board can be of the form {Board, node} or a Pid.
  """
  def add(:no_board, _, _board), do: :ok
  def add(_board, _, :no_board), do: :ok
  def add(board1, :left_of, board2) do
    :ok = board2 |> GenServer.call {:add, board1, :left}
    :ok = board1 |> GenServer.call {:add, board2, :right}
  end
  def add(board1, :down_of, board2) do
    :ok = board2 |> GenServer.call {:add, board1, :down}
    :ok = board1 |> GenServer.call {:add, board2, :up}
  end
  def add(board1, :right_of, board2), do: add(board2, :left_of, board1)
  def add(board1, :up_of, board2), do: add(board2, :down_of, board1)

  @doc """
  Removes a board adjacent to the board that is passed in the parameters.
  Parameter board can be of the form {Board, node} or a Pid.
  """
  def remove(_, :no_board), do: :ok
  def remove(:left_of, board) do
    :ok = board |> GenServer.call {:remove, :left}
  end
  def remove(:right_of, board) do
    :ok = board |> GenServer.call {:remove, :right}
  end
  def remove(:up_of, board) do
    :ok = board |> GenServer.call {:remove, :up}
  end
  def remove(:down_of, board) do
    :ok = board |> GenServer.call {:remove, :down}
  end

  # GenServer callbacks

  @doc false
  def init(:ok) do
    Process.flag(:trap_exit, true)
    {:ok, %State{}}
  end

  @doc false
  def handle_call({:get, :left}, _from, state = %State{left: left}) do
    {:reply, left, state}
  end
  def handle_call({:get, :right}, _from, state = %State{right: right}) do
    {:reply, right, state}
  end
  def handle_call({:get, :up}, _from, state = %State{up: up}) do
    {:reply, up, state}
  end
  def handle_call({:get, :down}, _from, state = %State{down: down}) do
    {:reply, down, state}
  end
  
  def handle_call({:add, other_board, :left}, _from,
                  state = %State{left: :no_board}) do
    {:reply, :ok, %State{state | left: other_board}}
  end
  def handle_call({:add, other_board, :right}, _from,
                  state = %State{right: :no_board}) do
    {:reply, :ok, %State{state | right: other_board}}
  end
  def handle_call({:add, other_board, :up}, _from,
                  state = %State{up: :no_board}) do
    {:reply, :ok, %State{state | up: other_board}}
  end
  def handle_call({:add, other_board, :down}, _from,
                  state = %State{down: :no_board}) do
    {:reply, :ok, %State{state | down: other_board}}
  end
  
  def handle_call({:remove, :left}, _from, state = %State{}) do
    {:reply, :ok, %State{state | left: :no_board}}
  end
  def handle_call({:remove, :right}, _from, state = %State{}) do
    {:reply, :ok, %State{state | right: :no_board}}
  end
  def handle_call({:remove, :up}, _from, state = %State{}) do
    {:reply, :ok, %State{state | up: :no_board}}
  end
  def handle_call({:remove, :down}, _from, state = %State{}) do
    {:reply, :ok, %State{state | down: :no_board}}
  end
  
  def handle_call(:stop, _from, state = %State{}) do
    reply = :ok
    {:stop, :normal, reply, state}
  end
  def handle_call(_request, state) do
    {:reply, {:error, :not_supported}, state}
  end

  @doc false
  def terminate(_reason, %State{}) do
    # Notify manager it has to remove this board from all the adjacent boards
    :ok = BoardManager.notify_board_gone(Node.self)
  end
end

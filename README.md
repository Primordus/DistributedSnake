# DistributedSnake

Snake on multiple computers. Based on Erlang and Elixir.

# Starting snake

1. Install dependencies with `mix deps.get && mix deps.compile`
2. Start snake with the following command: `./start.sh NAME_OF_THIS_NODE`
3. Go to `localhost:4000` from your browser.

# TODO list

1. snake.erl: head + tail logic for the snake itself
2. insect.erl: everything involving the bug in the game
3. input.erl: reading input from keyboard
4. Add functions to snake.js to draw everything on screen.
5. Update mix.exs with registered processes.

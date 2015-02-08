# DistributedSnake

Snake on multiple computers. Based on Erlang and Elixir.

[TOC]

# Starting snake

1. Install dependencies with `mix deps.get`
2. Start Phoenix endpoint with `mix phoenix.server`
3. Go to `localhost:4000` from your browser.

# TODO list

1. snake.erl: head + tail logic for the snake itself
2. bug.erl: everything involving the bug in the game
3. gpio.erl: easy to use API for controlling GPIO pins of the Raspberry Pi
4. led.erl: controlling LED using GPIO pins
5. button.erl: reading a button use GPIO pins
6. input.erl: reading input from keyboard
7. game_tick.erl: game counter logic
8. msg_counter.erl: tool to showcase how many messages are being sent
9. connect.erl: for connecting the Erlang nodes together.
10. Supervisor tree

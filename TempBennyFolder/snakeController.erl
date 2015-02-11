-module(snakeController).

-export([
		event_loop/2]).




event_loop(Direction, SnakePID) ->
    %%io:format("snakeController waiting for msg ~n",[]),
    receive
		{pidSnake, NewSnakePID} ->
			io:format("snakeController received PID of SNAKE ~w~n", [NewSnakePID]),
			event_loop(Direction, NewSnakePID);

		timeStamp ->
            %%io:format("snakeController received timeStamp ~n",[]),
			if SnakePID =/= 0 -> 
				%%io:format("Sending signal to SNAKE ~w~n", [SnakePID]),
				SnakePID ! {direction, Direction} 
			end,
            event_loop(Direction, SnakePID);

		{direction, NewDirection} ->
            %%io:format("snakeController Direction ~w~n",[NewDirection]),
            event_loop(NewDirection, SnakePID);

        X ->
            io:format("snakeController received event: ~w~n",[X]),
            event_loop(Direction, SnakePID)
    end.

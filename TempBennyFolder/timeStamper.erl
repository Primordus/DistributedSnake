-module(timeStamper).
-export([init/1]).

do_ticks(0, _) ->
	io:format("Timer done~n", []);

do_ticks(NrOfCounts, SnakeControllerPID) ->
	receive
		{pidSnakeController, NewSnakeControllerPID} ->
			io:format("timeStamper received ~w~n", [NewSnakeControllerPID]),
			do_ticks(NrOfCounts - 1, NewSnakeControllerPID);
		{stop} ->
			io:format("ending~n", [])
	after 1000 ->	
		%%io:format("Timed out ~n", []),
		if SnakeControllerPID =/= 0 -> 
			%%io:format("Sending signal to ~w~n", [SnakeControllerPID]),
			SnakeControllerPID ! timeStamp
		end,
		do_ticks(NrOfCounts - 1, SnakeControllerPID) 
	end.






init(NrOfCounts) ->
  	do_ticks(NrOfCounts, 0).
  	%%%spawn(timer, do_tick, NrOfCounts).

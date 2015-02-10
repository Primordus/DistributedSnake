-module(inputController).

-export([start/0,init/0]).

start() ->
    spawn(inputController,init,[]).

init() ->
    S = gs:start(),
    %%W = 
 	gs:create(window,S,[{map,true},{keypress,true},
                            {buttonpress,true},{motion,true}]),

    event_loop(0).

event_loop(SnakeControllerPID) ->
	receive
		{pidSnakeController, NewSnakeControllerPID} ->
			io:format("timeStamper received ~w~n", [NewSnakeControllerPID]),
			event_loop(NewSnakeControllerPID);

        {_,_,keypress,_,[Direction | _]} ->			
            %%io:format("Recieved keypress: ~w~n",[Direction]),
			if SnakeControllerPID =/= 0 -> 
				%%io:format("inputController sending signal to ~w~n", [Direction]),
				SnakeControllerPID ! {direction, Direction}
			end,
			event_loop(SnakeControllerPID);

        {_,_,keypress,_,[H | _]} ->			
            io:format("Recieved: ~w~n",[H]),			
            event_loop(SnakeControllerPID)%%;

		%%unknown event
        %%X ->
        %%    io:format("received event: ~w~n",[X]),
        %%    event_loop(SnakeControllerPID)
    end.

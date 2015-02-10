-module(snake).

-export([
		event_loop/4]).



event_loop(GuiPID, X, Y, Snake) ->
    io:format("SNAKE coord  ~w,~w, guiPID ~w snake ~w ~n",[X, Y, GuiPID, Snake]),
    receive
		{direction, 'Down'} when GuiPID =/= 0 ->
            io:format("SNAKE received down ~n",[]),			
			NewGuiPID = GuiPID,NewX = X, NewY = Y + 1;
		{direction, 'Up'} when GuiPID =/= 0 ->
            io:format("SNAKE received up ~n",[]),
			NewGuiPID = GuiPID,NewX = X, NewY = Y - 1;
		{direction, 'Left'} when GuiPID =/= 0 ->
            io:format("SNAKE received left ~n",[]),
			NewGuiPID = GuiPID,NewX = X - 1, NewY = Y;
		{direction, 'Right'} when GuiPID =/= 0 ->
            io:format("SNAKE received right ~n",[]),
			NewGuiPID = GuiPID,NewX = X + 1, NewY = Y; 
		{pidGui, NewGuiPID2} ->
			io:format("SNAKE received PID of gui ~w~n", [NewGuiPID2]),
			NewGuiPID = NewGuiPID2, NewX = X, NewY = Y;
        Xmsg ->
            io:format("SNAKE received event: ~w~n",[Xmsg]),
			NewGuiPID = GuiPID, NewX = X, NewY = Y
    end,
	if  
		length(Snake) > 4 -> [Remove | SnakeX] = Snake;
		true -> 
			SnakeX = Snake,
			Remove = {-1, -1}
	end,
    io:format("SNAKE coord  ~w   ~w, ~w~n",[SnakeX, NewX, NewY]),
	NewSnake = SnakeX ++ [{NewX, NewY}],
	NewSnake2 = lists:sublist(NewSnake, 1, 5),
	
	io:format("doTest Loop ~w, ~w ~n",[NewSnake2, Remove]), 

	if NewGuiPID =/= 0 ->
		NewGuiPID ! {headPos, NewX, NewY, Remove, NewSnake2}     	
	end,
	event_loop(NewGuiPID, NewX, NewY, NewSnake2).

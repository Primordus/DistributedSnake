-module(gui).


-export([start/0,init/0]).

start() -> spawn(gui, init, []).

init() ->
    I=gs:start(),
    Win=gs:create(window, I,
                  [{width, 400},{height, 400},
                   {title,"Snake"},{map, true}]),
    gs:create(canvas, can1,Win,
	      [{x,0},{y, 0},{width,400},{height,400},
	       {default,oval,{fill,green}}]),
    drawBoard(10, 10),
	%%drawRectRed(2,2)
    loop()
    .

loop() ->
    receive
		{gs,_Id,destroy,_Data,_Arg} -> bye;
		{headPos, X, Y, Remove, NewSnake2} ->
            io:format("GUI received ~w ~w ~n",[X,Y]),
			drawRectRed(X, Y),
			{RemX, RemY} = Remove,
			drawRectGray(RemX, RemY),

			%%drawRectGray(2,2),
			loop()
    end.


drawBoard(-1, -1) ->
	ok;
drawBoard(Row, -1) ->
	drawBoard(Row - 1, 10);
drawBoard(Row, Col) ->
	drawRect(Row, Col),
	drawBoard(Row, Col - 1).

drawRect(X, Y) ->
	Offset = 50,
	XY = {Offset + X * 20, Offset + Y * 20},
    XY2 = {Offset + X * 20 + 19, Offset + Y * 20 + 19},
	
    gs:create(rectangle,can1,[{coords,[XY,XY2]}]).

drawRectRed(X, Y) ->
	Offset = 50,
	XY = {Offset + X * 20, Offset + Y * 20},
    XY2 = {Offset + X * 20 + 19, Offset + Y * 20 + 19},
	
    gs:create(rectangle,can1,[{fill, red},{coords,[XY,XY2]}]).

drawRectGray(X, Y) ->
	Offset = 50,
	XY = {Offset + X * 20, Offset + Y * 20},
    XY2 = {Offset + X * 20 + 19, Offset + Y * 20 + 19},
	
    gs:create(rectangle,can1,[{fill, gray},{coords,[XY,XY2]}]).



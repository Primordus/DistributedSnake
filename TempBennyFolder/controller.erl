-module(controller).

-export([start/0]).

start() ->
	TimeStamperPID = spawn(timeStamper,init,[30]),	    
	InputControllerPID = spawn(inputController,init,[]),
	SnakeControllerPID = spawn(snakeController,event_loop,[0,0]),
	SnakePID = spawn(snake,event_loop,[0, 0, 0, [] ]),	
	GuiPID = gui:start(),

	%%send the PID of the SnakeController to the timeStamper
	TimeStamperPID ! {pidSnakeController, SnakeControllerPID},
	InputControllerPID ! {pidSnakeController, SnakeControllerPID},
	
	SnakeControllerPID ! {pidSnake, SnakePID},
	SnakePID  ! {pidGui, GuiPID}
	.


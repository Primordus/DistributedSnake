-module(snake_app).
-behavior(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Supervisor = snake_sup:start_link(),
    {ok, Supervisor}.

stop(_Args) ->
    ok.

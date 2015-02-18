-module(led).
-behavior(gen_server).
%% API:
-export([start_link/1, stop/1, on/1, off/1]).

%% Callback functions
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {pin = no_pin}).

-define(SERVER, ?MODULE).


%% API

%% Starts a LED process.
start_link(Pin) when Pin > 0 ->
    Args = Pin,
    Options = [],
    gen_server:start_link(?SERVER, Args, Options).

%% Stops a LED process.
stop(LED) -> ok = gen_server:call(LED, stop).

%% Turns the LED on.
on(LED) when is_pid(LED) -> ok = gen_server:call(LED, on).

%% Turns the LED off.
off(LED) when is_pid(LED) -> ok = gen_server:call(LED, off).



%% Callback functions

init(Pin) -> 
    process_flag(trap_exit, true),
    gpio:pin_mode(Pin, output),
    {ok, #state{pin = Pin}}.

handle_call(on, _From, State = #state{pin = Pin}) ->
    Reply = gpio:digital_write(Pin, high),
    {reply, normal, Reply, State};
handle_call(off, _From, State = #state{pin = Pin}) ->
    Reply = gpio:digital_write(Pin, off),
    {reply, normal, Reply, State};
handle_call(stop, _From, State = #state{}) ->
    {stop, normal, State}.

handle_cast(_Request, State = #state{}) -> {noreply, State}.

handle_info(_Request, State = #state{}) -> {noreply, State}.

terminate(_Reason, #state{pin = Pin}) -> gpio:pin_release(Pin).

code_change(_OldVersion, State, _Extra) -> {ok, State}.

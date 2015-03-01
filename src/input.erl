-module(input).


%% API:
-export([start_link/1, read/1]).


%% test from command-prompt:
%% > cd DistributedSnake/src
%% > sudo erl
%% > c(input.erl).
%% > {ok, InputSvr} = input:start_link(18).
%% > InputState = input:read(InputSvr).
%%   > low
%% > input:stop(InputSvr).  %%does not work yet...


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

%% Read the input pin.
read(InputSvr) when is_pid(InputSvr) -> 
    gen_server:call(InputSvr, read).


%% Callback functions

init(Pin) -> 
    process_flag(trap_exit, true),
    gpio:pin_mode(Pin, input),
    {ok, #state{pin = Pin}}.

handle_call(read, _From, State = #state{pin = Pin}) ->
    Reply = gpio:digital_read(Pin),
    {reply, Reply, State};


handle_call(stop, _From, State = #state{}) ->
    {stop, normal, State}.

handle_cast(_Request, State = #state{}) -> {noreply, State}.

handle_info(_Request, State = #state{}) -> {noreply, State}.

terminate(_Reason, #state{pin = Pin}) -> gpio:pin_release(Pin).

code_change(_OldVersion, State, _Extra) -> {ok, State}.

-module(led).
-behavior(gen_server).
%% API:
-export([start_link/1, stop/1, on/1, off/1]).

%% test from command-prompt:
%% > cd DistributedSnake/src
%% > sudo erl
%% > c(led.erl).
%% > {ok, LedSvr} = led:start_link(18).
%% > led:on(LedSvr).
%% > led:off(LedSvr).
%% > led:stop(LedSvr).  %%does not work yet...




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
stop(LedSvr) when is_pid(LedSvr) ->  
    ok = gen_server:call(LedSvr, stop).
    %%gen_event:stop(LED).

%% Turns the LED on.
on(LedSvr) when is_pid(LedSvr) -> 
    gen_server:call(LedSvr, on).

%% Turns the LED off.
off(LedSvr) when is_pid(LedSvr) -> 
    ok = gen_server:call(LedSvr, off).



%% Callback functions

init(Pin) ->     
    process_flag(trap_exit, true),
    gpio:pin_mode(Pin, output),
    {ok, #state{pin = Pin}}.

handle_call(on, _From, State = #state{pin = Pin}) ->
    Reply = gpio:digital_write(Pin, high),
    {reply, Reply, State};
handle_call(off, _From, State = #state{pin = Pin}) ->
    Reply = gpio:digital_write(Pin, low),
    {reply, Reply, State};
handle_call(stop, _From, State = #state{}) ->
    {stop, normal, State}.

handle_cast(_Request, State = #state{}) -> {noreply, State}.

handle_info(_Request, State = #state{}) -> {noreply, State}.

terminate(_Reason, #state{pin = Pin}) -> 
    gpio:pin_release(Pin).

code_change(_OldVersion, State, _Extra) -> {ok, State}.

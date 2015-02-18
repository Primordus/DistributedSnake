-module(input).


%% API:
-export([start_link/1, read/2]).

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
read(Server, Pin) when is_pid(Server) -> gen_server:call(Server, {read, Pin}).



%% Callback functions

init(Pin) -> 
    process_flag(trap_exit, true),
    gpio:pin_mode(Pin, input),
    {ok, #state{pin = Pin}}.

handle_call({read, Pin}, _From, _) ->
    Reply = gpio:digital_read(Pin),
    {reply, normal, Reply};

handle_call(stop, _From, State = #state{}) ->
    {stop, normal, State}.

handle_cast(_Request, State = #state{}) -> {noreply, State}.

handle_info(_Request, State = #state{}) -> {noreply, State}.

terminate(_Reason, #state{pin = Pin}) -> gpio:pin_release(Pin).

code_change(_OldVersion, State, _Extra) -> {ok, State}.

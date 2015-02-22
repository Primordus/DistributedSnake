-module(ticker).
-behavior(gen_server).

%% ticker API

-export([start_link/0, start_link/1, subscribe/1, unsubscribe/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_DELAY, 1000). % 1 second.

-record(state, {delay = ?DEFAULT_DELAY, pubsub = no_pid}).

%% API

%% Starts a ticker process with the default delay (1 second).
start_link() -> start_link(?DEFAULT_DELAY).

%% Starts a ticker process with a certain delay.
start_link(Delay) when Delay > 0 ->
    RegisteredName = {global, ?SERVER}, % 1 ticker process across entire cluster!
    Args = {delay, Delay},
    Options = [],
    gen_server:start_link(RegisteredName, ?MODULE, Args, Options). % TODO check what this gives when started on 2nd node => process result

%% Subscribes a process to the ticker process to start receiving ticks.
subscribe(SubFunction) when is_function(SubFunction, 1) ->
    gen_server:cast(?SERVER, {subscribe, SubFunction}).

%% Unsubscribes a process from the ticker process.
unsubscribe() -> gen_server:cast(?SERVER, {unsubscribe, self()}).

%% gen_server callback functions

init({delay, Delay}) ->
    process_flag(trap_exit, true),

    {ok, PubSub} = pubsub:start_link(),
    State = #state{delay = Delay, pubsub = PubSub},
    spawn_worker(Delay),
    {ok, State}.

handle_call(_Request, _From, State = #state{}) ->
    Reply = {error, not_supported},
    {reply, Reply, State}.

handle_cast({subscribe, SubFunction}, State = #state{pubsub = PubSub}) ->
    pubsub:add_sub(PubSub, SubFunction),
    {noreply, State};
handle_cast({unsubscribe, Pid}, State = #state{pubsub = PubSub}) ->
    pubsub:remove_sub(PubSub, Pid),
    {noreply, State};
handle_cast(time_expired, State = #state{delay = Delay, pubsub = PubSub}) ->
    pubsub:publish(PubSub, tick),
    spawn_worker(Delay),
    {noreply, State}.

handle_info(_Request, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Helper functions

spawn_worker(Delay) ->
    spawn(fun() ->
        timer:sleep(Delay),
        gen_server:cast(?SERVER, time_expired)
    end).

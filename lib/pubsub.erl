-module(pubsub).
-behavior(gen_event).

%% public API:
-export([start/0, stop/1, publish/2, add_sub/1, remove_sub/1]).

%% gen_event callbacks:
-export([init/1, handle_event/2, handle_call/2, handle_info/2, 
         terminate/2, code_change/3]).

%% Simple PubSub mechanism.

%% API

%% Starts the event manager (not included in a supervision tree).
start() -> 
    {ok, Manager} = gen_event:start(),
    Args = ok,
    gen_event:add_handler(Manager, ?MODULE, Args),
    {ok, Manager}.

%% Publishes a message to all subscribers.
publish(EventManager, Message) ->
    gen_event:sync_notify(EventManager, {publish, Message}).

%% Adds the calling process as a subscriber to the pubsub mechanism.
add_sub(EventManager) -> 
    gen_event:sync_notify(EventManager, {add_sub, self()}).

%% Removes the calling process from the pubsub mechanism.
remove_sub(EventManager) -> 
    gen_event:sync_notify(EventManager, {remove_sub, self()}).

%% Stops the event manager.
stop(EventManager) -> gen_event:stop(EventManager).

%% gen_event callbacks:

init(ok) -> {ok, []}. %% begin state = empty list of subscribers.

handle_event({publish, Message}, Subscribers) ->
    [Sub ! Message || Sub <- Subscribers],
    {ok, Subscribers};
handle_event({add_sub, Subscriber}, Subscribers) ->
    NewSubs = add(Subscribers, Subscriber),
    {ok, NewSubs};
handle_event({remove_sub, Subscriber}, Subscribers) ->
    {ok, lists:delete(Subscriber, Subscribers)}.

handle_call(_Request, _State) -> ok. % not used.
handle_info(_Request, _State) -> ok. % not used.

terminate(_Arg, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% Helper functions:

%% Add only to list if not already in list.
add(Subscribers, Subscriber) -> do_add(Subscribers, Subscriber, Subscribers).

do_add([], Sub, Subs) -> [Sub | Subs];
do_add([Sub | _RestSubs], Sub, Subs) -> Subs;
do_add([_OtherSub | RestSubs], Sub, Subs) -> do_add(RestSubs, Sub, Subs).

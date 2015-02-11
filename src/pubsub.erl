-module(pubsub).
-behavior(gen_event).

%% public API:
-export([start/0, stop/1, publish/2, 
        add_sub/2, add_sub/1, 
        remove_sub/1]).

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
publish(EventManager, Message) when is_pid(EventManager) ->
    gen_event:sync_notify(EventManager, {publish, Message}).

%% Adds a function that can process an incoming message as a subscriber to the 
%% pubsub mechanism. This can be used to e.g. better isolate your code in a 
%% single module. The subscribe function itself should do as little work as 
%% possible (for example forwarding a message).
%%
%% Example:
%% 
%% ... 
%% SelfPid = self(),
%% pubsub:add_sub(EvtMgr, fun(Msg) -> 
%%      gen_server:call(SelfPid, {new_msg, Msg}) 
%% end).
%% ...
%% 
%% handle_call({new_msg, Msg}, From, State) -> ...
%%
add_sub(EventManager, SubscribeFunction) 
        when is_pid(EventManager) 
        andalso is_function(SubscribeFunction, 1) ->
    gen_event:sync_notify(EventManager, {add_sub, self(), SubscribeFunction}).

%% Adds the calling process as a subscriber to the pubsub mechanism.
%% Incoming messages are delivered as is (using the ! operator).
add_sub(EventManager) -> 
    Pid = self(),
    F = fun(Message) ->
        Pid ! Message
    end,
    add_sub(EventManager, F).

%% Removes the calling process from the pubsub mechanism, should be the same
%% process as the one that subscribed to the pubsub process.
remove_sub(EventManager) when is_pid(EventManager)  -> 
    gen_event:sync_notify(EventManager, {remove_sub, self()}).

%% Stops the event manager.
stop(EventManager) when is_pid(EventManager) -> gen_event:stop(EventManager).

%% gen_event callbacks:

init(ok) -> {ok, #{}}. % Begin state = empty map.

handle_event({publish, Message}, SubscriberMap) ->
    % TODO check next line performance, 
    % or use maps:map if too slow
    [Func(Message) || Func <- maps:values(SubscriberMap)],
    {ok, SubscriberMap};
handle_event({add_sub, PidKey, Func}, Subscribers) ->
    {ok, maps:put(PidKey, Func, Subscribers)};

handle_event({remove_sub, PidKey}, Subscribers) ->
    {ok, maps:remove(PidKey, Subscribers)}.

handle_call(_Request, _State) -> ok. % not used.
handle_info(_Request, _State) -> ok. % not used.

terminate(_Arg, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

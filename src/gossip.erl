-module(gossip).
-behavior(gen_server).

%% TODO make mechanism for detecting node removal better (maybe polling?)

%% API

-export([start_link/0, subscribe/1]).

%% gen server callbacks

-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(FIRST_NODE, 'luc@localhost').

-record(state, {pubsub = no_pid}).

%% API

%% Starts the gossip server.
start_link() ->
    RegisteredName = {local, ?SERVER},
    Args = ok,
    Options = [],
    gen_server:start_link(RegisteredName, ?MODULE, Args, Options).

%% Subscribes a certain process to this gossip server with a certain function.
subscribe(SubFunction) when is_function(SubFunction, 1) ->
    ok = gen_server:call(?SERVER, {subscribe, SubFunction}).

%% Gen server callbacks

init(_Args = ok) ->
    process_flag(trap_exit, true),

    Result = net_adm:ping(?FIRST_NODE),
    ok = process_ping(Result), % crashes if ping fails.
    
    {ok, PubSub} = pubsub:start_link(),
    {ok, #state{pubsub = PubSub}}.

handle_call({subscribe, SubFunction}, _From, State = #state{pubsub = EvtMgr}) ->
    pubsub:add_sub(EvtMgr, SubFunction),
    Reply = ok,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = {error, not_supported},
    {reply, Reply, State}.

handle_cast(Event = {added_node, _Node}, State = #state{pubsub = EvtMgr}) -> 
    pubsub:publish(EvtMgr, Event),
    {noreply, State};
handle_cast(Event = {removed_node, _Node}, State = #state{pubsub = EvtMgr}) ->
    pubsub:publish(EvtMgr, Event),
    {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> 
    notify_node_removed(),
    ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% Helper functions

%% Process the result of the ping to another node.
process_ping(pong) ->
    Node = node(),
    % spawns process on the first node (knows all other nodes already), 
    % notifies everybody except this node of the change in the cluster.
    % TODO improve this code later..
    spawn(?FIRST_NODE, fun() -> notify_node_added(Node) end),
    ok;
process_ping(pang) ->
    not_ok.

%% Sends an async message to all nodes (except NewNode) that a new node is added.
notify_node_added(NewNode) ->
    Nodes = lists:delete(NewNode, [node() | nodes()]),
    gen_server:abcast(Nodes, ?SERVER, {added_node, NewNode}).

%% Sends an async message to all other nodes that a node is removed.
notify_node_removed() ->
    gen_server:abcast(nodes(), ?SERVER, {removed_node, node()}).

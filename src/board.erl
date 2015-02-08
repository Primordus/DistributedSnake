-module(board).
-behavior(gen_server).

%% board API functions
-export([start_link/0, stop/1, get_all/0, get/1, add/3, remove/2]).
% TODO also create add/2 and remove/1?

%% gen_server callback functions
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {up = no_board, down = no_board, 
                right = no_board, left = no_board}).

%% Board API
%% Due to the nature of this process, all requests are handled synchronously.

%% Starts a board process.
start_link() ->
    RegisteredName = {local, ?SERVER},
    Args = ok,
    Options = [],
    gen_server:start_link(RegisteredName, ?SERVER, Args, Options).

%% Stops a board process.
stop(Board) -> gen_server:call(Board, stop).

%% Gives back a list of all boards ([{Board, node()}].
get_all() -> [{?SERVER, Node} || Node <- [node() | nodes()]].

% Gives back the pid of an adjacent board (or no_board).
get(left) ->  gen_server:call(?SERVER, {get, left});
get(right) -> gen_server:call(?SERVER, {get, right});
get(up) ->    gen_server:call(?SERVER, {get, up});
get(down) ->  gen_Server:call(?SERVER, {get, down}).

%% BoardX = {?SERVER, node()}.
add(Board1, left_of, Board2) ->
    ok = gen_server:call(Board2, {add, Board1, left}),
    ok = gen_server:call(Board1, {add, Board2, right});
add(Board1, down_of, Board2) ->
    ok = gen_server:call(Board2, {add, Board1, down}),
    ok = gen_server:call(Board1, {add, Board2, up});
add(Board1, right_of, Board2) ->
    add(Board2, left_of, Board1);
add(Board1, up_of, Board2) ->
    add(Board2, down_of, Board1).

%% Removes a board
remove(_, no_board) ->     ok;
remove(left_of, Board) ->  ok = gen_server:call(Board, {remove, left});
remove(right_of, Board) -> ok = gen_server:call(Board, {remove, right});
remove(up_of, Board) ->    ok = gen_server:call(Board, {remove, up});
remove(down_of, Board) ->  ok = gen_server:call(Board, {remove, down}).

%% gen_server callbacks:

init(ok) ->
    process_flag(trap_exit, true),
    BeginState = #state{},
    {ok, BeginState}.

handle_call({get, left}, _From, State = #state{left = Left}) ->
    {reply, Left, State};
handle_call({get, right}, _From, State = #state{right = Right}) ->
    {reply, Right, State};
handle_call({get, up}, _From, State = #state{up = Up}) ->
    {reply, Up, State};
handle_call({get, down}, _From, State = #state{down = Down}) ->
    {reply, Down, State};

handle_call({add, OtherBoard, left}, _From, State = #state{left = no_board}) ->
    {reply, ok, State#state{left = OtherBoard}};
handle_call({add, OtherBoard, right}, _From, State = #state{right = no_board}) ->
    {reply, ok, State#state{right = OtherBoard}};
handle_call({add, OtherBoard, up}, _From, State = #state{up = no_board}) ->
    {reply, ok, State#state{up = OtherBoard}};
handle_call({add, OtherBoard, down}, _From, State = #state{down = no_board}) ->
    {reply, ok, State#state{down = OtherBoard}};

handle_call({remove, left}, _From, State = #state{}) ->
    {reply, ok, State#state{left = no_board}};
handle_call({remove, right}, _From, State = #state{}) ->
    {reply, ok, State#state{right = no_board}};
handle_call({remove, up}, _From, State = #state{}) ->
    {reply, ok, State#state{up = no_board}};
handle_call({remove, down}, _From, State = #state{}) ->
    {reply, ok, State#state{down = no_board}};

handle_call(stop, _From, State = #state{}) ->
    Reply = ok,
    {reply, normal, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State = #state{left = Left, right = Right, 
                                    up = Up, down = Down}) ->
    remove(left_of, Right),
    remove(right_of, Left),
    remove(down_of, Up),
    remove(up_of, Down),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

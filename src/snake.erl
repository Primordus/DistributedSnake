-module(snake).
-behavior(gen_server).

%% TODO check for errors (mostly move/collision part)!

%% API for snake module
-export([start_link/0, start_link/1, start_link/3]).

%% gen server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SUPERVISOR, 'Elixir.Snake.SnakeSupervisor').
-define(GUI, 'Elixir.Phoenix.Channel').

-define(WIDTH, 10). % TODO refactor this later to board module.
-define(HEIGHT, 10). % TODO refactor this later to board module.
-define(SCORE, 10). % TODO refactor this also (insect module?).

-record(state, {segment = head, 
                position = {0, 0}, 
                next_segment = no_pid,
                node = node()}).

%% API

%% Starts a snake process (head) at location {0, 0}.
start_link() -> start_link(head, 0, 0).

%% Starts a snake process (head or tail) at location {X, Y}.
start_link(head, X, Y) -> 
    Args = [head, X, Y],
    Options = [],
    gen_server:start_link(?MODULE, Args, Options);
start_link(tail, X, Y) ->
    Args = [tail, X, Y],
    Options = [],
    gen_server:start_link(?MODULE, Args, Options).

%% Starts a snake process with a certain state 
%% (only used by supervisor to move snake from node to node).
start_link([State = #state{}]) ->
    Args = [State#state{node = node()}], % Old state still contained old node?
    Options = [],
    gen_server:start_link(?MODULE, Args, Options).

%% gen server callbacks

init([head, X, Y]) ->
    Snake = self(),
    ticker:subscribe(fun(tick) -> gen_server:cast(Snake, update_state) end),
    State = #state{segment = head, position = {X, Y}},
    {ok, State};
init([tail, X, Y]) ->
    State = #state{segment = tail, position = {X, Y}},
    {ok, State};
init([State = #state{segment = head}]) ->
    Snake = self(),
    ticker:subscribe(fun(tick) -> gen_server:cast(Snake, update_state) end),   
    {ok, State};
init([State = #state{segment = tail}]) ->
    {ok, State}.

handle_call({is_collision, {X, Y, Node}}, _From, 
            State = #state{position = {X, Y}, node = Node}) ->
    % Same coordinates and node => collision!
    Reply = {is_collision, true},
    {reply, Reply, State};
handle_call({is_collision, {X, Y, Node}}, _From,
            State = #state{position = {X, Y},
                           next_segment = Next,
                           node = _OtherNode}) ->
    % Same coordinates, different nodes => no collision (here), check rest!
    Reply = is_collision(Next, X, Y, Node),
    {reply, Reply, State};
handle_call({is_collision, {X, Y, Node}}, _From, 
            State = #state{position = {_OtherX, _OtherY}, 
                           next_segment = Next}) ->
    % Different coordinates, different nodes => no collision (here), check rest!
    Reply = is_collision(Next, X, Y, Node),
    {reply, Reply, State};
handle_call({move, NewPosition, Node}, _From, 
            State = #state{next_segment = no_pid, 
                           node = Node}) ->
    % Only called in tail segments.
    % Previous part is on same node as this one, no special action needed.
    NewState = State#state{position = NewPosition},
    Reply = {tail, self()}, % pid stays the same => return self()!
    {reply, Reply, NewState};
handle_call({move, NewPosition, NewNode}, _From,
            State = #state{next_segment = no_pid}) ->
    % Only called in tail segments
    % Previous part is on another node => special action needed.
    % 1) Update state.
    NewState = State#state{position = NewPosition},
    % 2) Start the process on the new node with this new state.
    {ok, NewSegment} = ?SUPERVISOR:start_child(NewNode, NewState),
    % 3) Notify previous segment that this process is now on another node.
    % And finally stop this process.
    Reason = moving_to_other_node, 
    Reply = {tail, NewSegment},
    {stop, Reason, Reply, State};
handle_call({move, NewPosition, Node}, _From, 
            State = #state{position = OldPosition, 
                           next_segment = NextPid,
                           node = Node}) ->
    % Only called in tail segments.
    % Previous part is on same node as this one, no special action needed
    {tail, Segment} = gen_server:call(NextPid, {move, OldPosition, Node}),
    NewState = State#state{position = NewPosition, next_segment = Segment},
    Reply = {tail, self()},
    {reply, Reply, NewState};
handle_call({move, NewPosition, NewNode}, _From, 
            State = #state{position = OldPosition, 
                           next_segment = Next}) ->
    % Only called in tail segments
    % Previous part is on another node => special action needed.
    % 1) Notify next segment of movement
    {tail, Segment} = gen_server:call(Next, {move, OldPosition, node()}),
    % 2) Update state
    NewState = State#state{position = NewPosition, next_segment = Segment},
    % 3) Start the process on the new node with this new state.
    {ok, NewSegment} = ?SUPERVISOR:start_child(NewNode, NewState),
    % 4) Update previous segment that this segment is now on another node.
    % And finally stop this process.
    Reason = moving_to_other_node,
    Reply = {tail, NewSegment},
    {stop, Reason, Reply, NewState};
handle_call(_Request, _From, State) ->
    Reply = {error, not_supported},
    {reply, Reply, State}.

handle_cast(update_state, State = #state{}) ->
    % Only triggered in the head! => updates the entire snake recursively!
    % Ask for new input.
    Direction = input:get_direction(),
    
    % Move the snake.
    Result = move(Direction, State),
    handle_move(Result),
    Result; % TODO try merging these 2 handle_casts back together!
handle_cast(update_state2, State = #state{position = {NewX, NewY}, 
                                          next_segment = Next,
                                          node = Node}) ->
    % Check for collisions with itself
    {is_collision, SnakeCollision} = is_collision(Next, NewX, NewY, Node),
    handle_collision(SnakeCollision),

    % Check for collisions with insect => if yes: update score
    InsectLocation = insect:get_location(),
    {is_collision, InsectCollision} = is_insect_collision(InsectLocation, 
                                                          State),
    update_score(?SCORE, InsectCollision),

    % Draw the snake.
    draw(State),
    {noreply, State};
handle_cast(draw, State = #state{}) ->
    draw(State),
    {noreply, State};
handle_cast(gameover, State = #state{next_segment = no_pid}) ->
    {stop, gameover, State};
handle_cast(gameover, State = #state{next_segment = Next}) ->
    gen_server:cast(Next, gameover),
    {stop, gameover, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, #state{segment = head}) -> 
    ticker:unsubscribe(),
    ok;
terminate(_Reason, _State = #state{}) ->
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Helper functions

%% GUI related
draw(#state{position = {X, Y}, next_segment = no_pid}) -> 
    draw_segment(X, Y);
draw(#state{position = {X, Y}, next_segment = NextPid}) ->
    draw_segment(X, Y),
    gen_server:cast(NextPid, draw). % TODO make this a call?

draw_segment(X, Y) -> ?GUI:broadcast("draw_snake", {X, Y}).

update_score(_Score, no_collision) -> ok;
update_score(Score, collision) -> do_update_score(Score).

do_update_score(Score) ->  ?GUI:broadcast("score", Score).

%% Other helpers

% Compares insect location with head of snake location:
is_insect_collision({X, Y, Node}, #state{position = {X, Y}, node = Node}) ->
    collision;
is_insect_collision(_InsectLocation, _SnakeState) ->
    no_collision.

move(Direction, State = #state{next_segment = no_pid}) -> 
    do_move(Direction, State);
move(Direction, State = #state{position = {OldX, OldY}, 
                               next_segment = Next,
                               node = Node}) ->
    % next function has to be synchronous, 
    % otherwise collision logic might be buggy.
    {tail, Segment} = gen_server:call(Next, {move, {OldX, OldY}, Node}),    
    NewState = State#state{next_segment = Segment},
    do_move(Direction, NewState).

do_move(left, State = #state{position = {X, Y}}) when X > 0 ->
    NewState = State#state{position = {X - 1, Y}},
    {noreply, NewState};
do_move(left, State = #state{position = {_X, Y}}) ->
    {board, NewNode} = board:get(left),
    NewState = State#state{position = {?WIDTH, Y}},
    NewHead = spawn_head(NewNode, NewState),
    Reason = {moving_to_other_node, NewHead},
    {stop, Reason, State};
do_move(right, State = #state{position = {X, Y}}) when X < ?WIDTH ->
    NewState = State#state{position = {X + 1, Y}},
    {noreply, NewState};
do_move(right, State = #state{position = {_X, Y}}) ->
    {board, NewNode} = board:get(right),
    NewState = State#state{position = {0, Y}},
    NewHead = spawn_head(NewNode, NewState),
    Reason = {moving_to_other_node, NewHead},
    {stop, Reason, State};
do_move(up, State = #state{position = {X, Y}}) when Y < ?HEIGHT ->
    NewState = State#state{position = {X, Y + 1}},
    {noreply, NewState};
do_move(up, State = #state{position = {X, _Y}}) ->
    {board, NewNode} = board:get(up),
    NewState = State#state{position = {X, 0}},
    NewHead = spawn_head(NewNode, NewState),
    Reason = {moving_to_other_node, NewHead},
    {stop, Reason, State};
do_move(down, State = #state{position = {X, Y}}) when Y > 0 ->
    NewState = State#state{position = {X, Y - 1}},
    {noreply, NewState};
do_move(down, State = #state{position = {X, Y}}) ->
    {board, NewNode} = board:get(down),
    NewState = State#state{position = {X, Y - 1}},
    NewHead = spawn_head(NewNode, NewState),
    Reason = {moving_to_other_node, NewHead},
    {stop, Reason, State}.

spawn_head(no_board, _State) -> 
    gen_server:cast(self(), gameover); % TODO make this a call, otherwise the {stop, ...} prevents game from cleaning up?
spawn_head(Node, State) -> 
    {ok, Head} = ?SUPERVISOR:start_child(Node, State),
    Head.

handle_move({noreply, _State}) ->
    gen_server:cast(self, update_state2);
handle_move({stop, {_Reason, NewHead}, _State}) ->
    gen_server:cast(NewHead, update_state2).

is_collision(no_pid, _X, _Y, _Node) -> {is_collision, false};
is_collision(NextPid, X, Y, Node) -> 
    % TODO maybe use cast here later? 
    % splits main game logic up in 2 parts though..
    gen_server:call(NextPid, {is_collision, {X, Y, Node}}).

handle_collision(false) -> ok;
handle_collision(true) -> gen_server:cast(self(), gameover).

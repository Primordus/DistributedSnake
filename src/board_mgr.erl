-module(board_mgr).
-behavior(gen_server).

%% TODO ASCII art for better explanation of movement.

%% This module contains code for a process that can dynamically connect and 
%% remove board processes from each other.

%% API

-export([start_link/0, notify_board_gone/1]).

%% Gen server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {position = {0, 0},
                last_direction = right, 
                steps_left = 2,
                step_counter = 1, % memorizes how many steps per size of spiral
                gaps = [],
                old_position_saved = false}).

%% API

%% Starts a board manager process.
start_link() ->
    RegisteredName = {global, ?SERVER}, % 1 process for entire application.
    Args = ok,
    Options = [],
    gen_server:start_link(RegisteredName, ?MODULE, Args, Options).

%% Notifies the manager that a board is gone.
notify_board_gone(Node) -> gen_server:call(?SERVER, {removed_node, Node}).

%% Gen server callbacks

init(ok) ->
    process_flag(trap_exit, true),
    board_db:init(),
    gossip:subscribe(fun
            (Msg = {added_node, _Node}) ->
                ok = gen_server:call(?SERVER, Msg);
            ({removed_node, Node}) ->
                ok = notify_board_gone(Node)
    end),
    {ok, #state{}}.

handle_call({added_node, Node}, _From, State = #state{position = Position}) ->
    add_board(Position, Node),
    NewState = calc_new_state(State),
    {reply, ok, NewState};
handle_call({removed_node, Node}, _From, State = #state{}) ->
    {removed, Position} = remove_board(Node),
    NewState = add_gaps(Position, State),
    {reply, ok, NewState};
handle_call(_Request, _From, State = #state{}) ->
    Reply = {error, not_supported},
    {reply, Reply, State}.

handle_cast(_Request, State = #state{}) ->
    {noreply, State}.

handle_info(_Request, State = #state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #state{}) -> 
    gossip:unsubscribe(),
    board_db:destroy(),
    ok.

code_change(_OldVsn, State = #state{}, _Extra) -> {ok, State}.

%% Helper functions

%% Adds a board to the entire setup
add_board(Position = {X, Y}, Node) ->
    NewBoard = node_to_board(Node),
    board_db:add(Position, Node),

    Up = board_db:get({X, Y + 1}),
    Down = board_db:get({X, Y - 1}),
    Right = board_db:get({X + 1, Y}),
    Left = board_db:get({X - 1, Y}),
    
    board:add(Up, up_of, NewBoard),
    board:add(Down, down_of, NewBoard),
    board:add(Right, right_of, NewBoard),
    board:add(Left, left_of, NewBoard),
    
    ok.

%% Removes a board from the entire setup
%% Note: This node is already gone from the cluster!
remove_board(Node) ->
    Position = board_db:get_key(Node),
    do_remove(Position, Node).

do_remove(no_position, _Node) -> 
    {removed, no_position};
do_remove(Position = {X, Y}, _Node) ->
    board_db:remove(Position),

    Up = board_db:get({X, Y + 1}),
    Down = board_db:get({X, Y - 1}),
    Right = board_db:get({X + 1, Y}),
    Left = board_db:get({X - 1, Y}),

    board:remove(left_of, Right),
    board:remove(right_of, Left),
    board:remove(down_of, Up),
    board:remove(up_of, Down),

    {removed, Position}.

add_gaps(no_position, State) -> 
    State;
add_gaps(Position, State) ->
    % Gaps ordered from old to new (oldest is first element)
    NewGaps = append_to_list(State#state.gaps, Position),
    State#state{gaps = NewGaps}.

%% Makes a spiral movement for adding boards, with priority to filling gaps.
%% TODO test this with unit test later!!!
calc_new_state(State = #state{position = OldPosition,
                              last_direction = OldDirection, 
                              steps_left = 0,
                              step_counter = Steps,
                              gaps = []}) ->
    % Special case: change direction + update steps and step_counter.
    NewDirection = change_direction(OldDirection),
    NewPosition = calc_position(OldPosition, NewDirection),
    State#state{position = NewPosition,
                last_direction = NewDirection,
                steps_left = 2 * (Steps + 1),
                step_counter = Steps + 1,
                old_position_saved = false};
calc_new_state(State = #state{position = OldPosition,
                              last_direction = OldDirection, 
                              steps_left = Steps,
                              step_counter = Steps,
                              gaps = []}) ->
    % Special case: first direction change 
    % (+ update steps and step counter).
    NewDirection = change_direction(OldDirection),
    NewPosition = calc_position(OldPosition, NewDirection),
    State#state{position = NewPosition,
                last_direction = NewDirection,
                steps_left = Steps - 1,
                old_position_saved = false};
calc_new_state(State = #state{position = OldPosition,
                              last_direction = Direction,
                              steps_left = Steps,
                              gaps = []}) ->
    % Normal case: move in same direction and update steps.
    NewPosition = calc_position(OldPosition, Direction),
    State#state{position = NewPosition,
                steps_left = Steps - 1,
                old_position_saved = false};
calc_new_state(State = #state{position = OldPosition,
                              gaps = [First | Rest],
                              old_position_saved = false}) ->
    % Special case: gaps => fill up oldest one first!
    % Bump old position to end of list!
    NewGaps = append_to_list(Rest, OldPosition),
    State#state{position = First,
                gaps = NewGaps,
                old_position_saved = true};
calc_new_state(State = #state{gaps = [First | Rest],
                              old_position_saved = true}) ->
    % Special case: gaps => fill up oldest one first!
    State#state{position = First, gaps = Rest}.

change_direction(right) -> down;
change_direction(down) -> left;
change_direction(left) -> up;
change_direction(up) -> right.

calc_position({X, Y}, right) -> {X + 1, Y};
calc_position({X, Y}, down) -> {X, Y - 1};
calc_position({X, Y}, up) -> {X, Y + 1};
calc_position({X, Y}, left) -> {X - 1, Y}.

append_to_list(List, Item) ->
    ReversedList = lists:reverse(List),
    NewList = [Item | ReversedList],
    lists:reverse(NewList).

node_to_board(Node) -> {board, Node}.

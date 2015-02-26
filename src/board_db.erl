-module(board_db).
-export([init/0, destroy/0, add/2, remove/1, get/1, get_key/1]).

%% Module that uses ETS storage to store the location of the boards in RAM.
%% Key-value store, key = position, value = a node (with a board process).

-define(TABLE_NAME, ?MODULE).
-define(TABLE_OPTIONS, [named_table, %% Use an atom to address the ETS-table
                        private,     %% Only owner can read/write from/to table
                        {keypos, 1}, %% Key at position 1 of the tuple (1-based)
                        set]).       %% No duplicates allowed!

%% API:

%% Initializes the table.
init() -> ets:new(?TABLE_NAME, ?TABLE_OPTIONS).

%% Adds a board at position {X, Y} to the table.
add(Key = {_X, _Y}, Board) ->
    % pattern match fails if board already present at {X, Y}.
    true = ets:insert_new(?TABLE_NAME, {Key , Board}). 

%% Removes a board at position {X, Y} from the table.
remove(Key = {_X, _Y}) -> ets:delete(?TABLE_NAME, Key).

%% Searches for a board at position {X, Y} from the table.
%% Returns the node if a matching record is found, or no_board if none found.
get(Key = {_X, _Y}) ->
    Result = ets:lookup(?TABLE_NAME, Key),
    handle_get_result(Result).

%% Finds the corresponding position for a certain board.
get_key(Board) ->
    Result = ets:match(?TABLE_NAME, {'$1', Board}),
    handle_get_key_result(Result).

%% Destroys the entire table.
destroy() -> ets:delete(?TABLE_NAME).

%% Helper functions:

handle_get_result([]) ->     no_board;
handle_get_result([{_Key, Node}]) -> Node.

handle_get_key_result([]) -> no_position;
handle_get_key_result([Position]) -> Position.

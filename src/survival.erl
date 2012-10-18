%%%---------------------------------------------------------------------
%%% Description module survival
%%%---------------------------------------------------------------------
%%% Contains main logic for starting and running the game.
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% start()
%%%   starts the game
%%%---------------------------------------------------------------------


-module(survival).
-author("Brian E. Williams").

-compile([debug_info]).

-export([start/0]).

start() -> start("Player_Name").

start(PlayerName) ->
    Map = map:default_map(),                           % list of lists of terrain atoms
	Xloc = random:uniform(length(lists:nth(1, Map))),  % random xloc across the top row of the map
	Player = player:new_player(PlayerName, Xloc),      % new player record
    % create the state machine for tracking game state
	game_loop({Map, Player}).

game_loop({Map, Player}) ->
	% display map or combat
    % get input from player
    % handle input
    % update player, fsm
    % check for end of game
    % loop
    game_loop({Map, Player}).



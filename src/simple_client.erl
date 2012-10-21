%% Author: bwilliams
%% Created: Oct 20, 2012
%% Description: TODO: Add description to simple_client
-module(simple_client).

%%
%% Include files
%%

-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
-endif.

%%
%% Exported Functions
%%
-export([start/1, quit/1, display_legend/1, display_status/1,
		 display_map/1, choose_direction/2, choose_weapon/2]).

%%
%% API Functions
%%

start(PlayerName) ->
	{ok, Game} = survival_fsm:start(PlayerName),
	Game.

quit(FSM) ->
	survival_fsm:quit(FSM).

display_map(FSM) ->
    survival_fsm:send_display_map(FSM).

display_status(FSM) ->
    survival_fsm:send_display_status(FSM).

display_legend(FSM) ->
    survival_fsm:send_display_legend(FSM).

choose_direction(FSM, Direction) ->
	survival_fsm:send_direction_choice(FSM, Direction).

choose_weapon(FSM, Weapon) ->
	survival_fsm:send_weapon_choice(FSM, Weapon).


%%
%% Local Functions
%%

%% --------------------------------------------------------------------
%% eunit tests
%% --------------------------------------------------------------------
-ifdef(TEST).
  start_quit_test() ->
	%start up the game
	Game = simple_client:start("Simple Client"),
	?assert(is_pid(Game)),
	?assert(is_process_alive(Game)),
	
	% now shut it down
	ok = simple_client:quit(Game),
	?assertNot(is_process_alive(Game)).
-endif.

%% Author: bwilliams
%% Created: Oct 20, 2012
%% Description: Methods for sending events to the game FSM from the shell,
%%  mostly for testing purposes.
-module(shell_client).

%%
%% Usage:
%%   Game = shell_client:start("Name").
%%   shell_client:choose_direction(Game, 5).
%%   shell_client:done(Game).
%%   shell_client:choose_weapon(Game, 5).
%%   shell_client:quit(Game).

%%
%% Include files
%%

-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
-endif.

%%
%% Exported Functions
%%
-export([start/1, quit/1, display_legend/1, display_status/1, done/1,
		 display_map/1, choose_direction/2, choose_weapon/2, get_state/1]).

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

done(FSM) ->
	survival_fsm:send_done(FSM).

choose_direction(FSM, Direction) when is_integer(Direction) ->
	survival_fsm:send_direction_choice(FSM, Direction).

choose_weapon(FSM, Weapon) when is_integer(Weapon) ->
	survival_fsm:send_weapon_choice(FSM, Weapon).

get_state(FSM) ->
	{ok, State} = survival_fsm:get_client_state(FSM),
	State.


%%
%% Local Functions
%%

%% --------------------------------------------------------------------
%% eunit tests
%% --------------------------------------------------------------------
-ifdef(TEST).
start_quit_test() ->
	Game = shell_client:start("Simple Client"),
	%start up the game
	?assert(is_pid(Game)),
	?assert(is_process_alive(Game)),
	
	% now shut it down
	ok = shell_client:quit(Game),
	?assertNot(is_process_alive(Game)).

display_test() ->
	Game = shell_client:start("Simple Client"),
	display_legend(Game),
	display_map(Game),
	display_status(Game),
	ok = shell_client:quit(Game).

send_commands_test() ->
	Game = shell_client:start("Simple Client"),
	
	% set a random seed for predictable game play
	random:seed(1, 1, 1000),
	ok = done(Game),
	ok = choose_direction(Game, 6),
	ok = choose_weapon(Game, 2),
    ok = shell_client:quit(Game).

get_state_test() ->
	Game = shell_client:start("Simple Client"),
	
	% set a random seed for predictable game play
	random:seed(1, 1, 1000),
	State = shell_client:get_state(Game),
	?assertNotEqual(ok, State).

-endif.

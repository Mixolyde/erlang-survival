%%% -------------------------------------------------------------------
%%% Author  : bwilliams
%%% Description : Tracks the state and plays the game 
%%%
%%% Created : Oct 16, 2012
%%% -------------------------------------------------------------------
-module(survival_fsm).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("survival.hrl").
-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
-endif.
%% --------------------------------------------------------------------
%% External exports
-export([start/1, start_link/1, quit/1]).
-export([send_direction_choice/2, send_weapon_choice/2, send_display_legend/1, send_display_status/1,
		 send_display_map/1]).

%% gen_fsm callbacks
-export([init/1, choose_direction/2, choose_direction/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {map, player, combat, day, time, scenario, options}).

%% ====================================================================
%% External functions
%% ====================================================================
start(PlayerName) ->
  gen_fsm:start(?MODULE, initial_state_params(PlayerName), []).
 
start_link(PlayerName) ->
  gen_fsm:start_link(?MODULE, initial_state_params(PlayerName), []).

quit(FSM) ->
	%terminate the FSM
    gen_fsm:sync_send_all_state_event(FSM, {quit}),
    ok.

%% Actual game interface commands
send_direction_choice(FSM, Direction) when is_integer(Direction) ->
    io:format("Sending ~b direction choice to FSM~n", [Direction]),
    gen_fsm:sync_send_event(FSM, {direction, Direction}).

send_weapon_choice(FSM, Weapon) when is_integer(Weapon) ->
	io:format("Sending ~b weapon choice to FSM~n", [Weapon]),
    gen_fsm:sync_send_event(FSM, {weapon, Weapon}).

send_display_map(FSM) ->
	io:format("Sending map request to FSM~n"),
    gen_fsm:sync_send_event(FSM, {display_map}).

send_display_status(FSM) ->
	io:format("Sending status request to FSM~n"),
    gen_fsm:sync_send_event(FSM, {display_status}).

send_display_legend(FSM) ->
	io:format("Sending legend request to FSM~n"),
    gen_fsm:sync_send_event(FSM, {display_legend}).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init([Player, Map]) ->
	FirstState = #state{map=Map, player=Player, combat={}, 
						day=1, time=am, scenario=basic, options=[]},
	notice(FirstState, "Initial FSM State created: ~p~n", [printable_state(FirstState)]),
    {ok, choose_direction, FirstState}.

%% --------------------------------------------------------------------
%% Func: choose_direction/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
choose_direction(_Event, StateData) ->
    {next_state, choose_direction, StateData}.

%% --------------------------------------------------------------------
%% Func: choose_direction/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
choose_direction({display_status}, _From, StateData) ->
	display_status(StateData),
    Reply = ok,
    {reply, Reply, choose_direction, StateData};
choose_direction({display_legend}, _From, StateData) ->
	display_legend(StateData),
    Reply = ok,
    {reply, Reply, choose_direction, StateData};
choose_direction({display_map}, _From, StateData) ->
	display_map(StateData),
    Reply = ok,
    {reply, Reply, choose_direction, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
	unexpected(Event, handle_event),
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event({quit}, _From, StateName, StateData) ->
    notice(StateData, "received quit event while in state ~w~n", [StateName]),
    {stop, normal, ok, printable_state(StateData)};
handle_sync_event(Event, _From, StateName, StateData) ->
	unexpected(Event, handle_sync_event),
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
	unexpected(Info, info),
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
	notice(StateData, "Terminating FSM while in state ~w because: ~w~n", 
		   [StateName, Reason]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
initial_state_params(PlayerName) ->
  Map = survival_map:default_map(),                           % list of lists of terrain atoms
  % random start location from possible starts
  Start = lists:nth(random:uniform(length(?STARTS)), ?STARTS),  
  Player = survival_player:new_player(PlayerName, Start),      % new player record
  [Player, Map].

% Send players a notice. This could be messages to their clients
% but for our purposes, outputting to the shell is enough.
notice(#state{player=#player{pname=N}}, Str, Args) ->
  io:format("~s: "++Str++"~n", [N|Args]).
 
% Logs unexpected messages
unexpected(Msg, State) ->
  io:format("~p received unknown event ~p while in state ~p~n",
  [self(), Msg, State]).

% Strip the map out of the state for quick debug printing
printable_state(StateData) ->
	StateData#state{map=[]}.

% internal call for displaying the map, in case of future changes
display_map(#state{map = Map, player = Player}) -> 
	survival_map:print_map_and_player(Map, Player).

display_legend(_StateData) ->
	survival_map:print_legend(),
	ok.

display_status(#state{day=Day, time=Time, player=#player{pname=Name, weapons=Weapons, ws=Wounds}}) ->
	io:format("Player: ~s~nDay-~b Time:~s WS:~b~n", 
			  [Name, Day, string:to_upper(atom_to_list(Time)), Wounds]),
	io:format("Weapons:~n"),
	[io:format("~s Rounds:~s~n", 
			   [Weap#weapon.displayname, 
				Weap#weapon.rounds]) || {_Ref, Weap} <- Weapons],
	ok.

%% --------------------------------------------------------------------
%% eunit tests
%% --------------------------------------------------------------------
-ifdef(TEST).
  test_start() ->
	{ok, Game} = survival_fsm:start("Survival FSM"),
	Game.

  test_end(Game) ->
	ok = survival_fsm:quit(Game).

  start_end_test() ->
	{ok, FSM} = survival_fsm:start("Survival FSM"),
	?assert(is_pid(FSM)),
	?assert(is_process_alive(FSM)),
	% sync call, shouldn't need time to shutdown
	ok = survival_fsm:quit(FSM),
	?assertNot(is_process_alive(FSM)).

  display_test() ->
	FSM = test_start(),
    send_display_legend(FSM),
	send_display_map(FSM),
	send_display_status(FSM),
	test_end(FSM).

display_status_test() ->
Player = survival_player:new_player(),
Map = survival_map:default_map(),
State = #state{map=Map, player=Player, combat={},
	day=1, time=am, scenario=basic, options=[]},

ok = display_status(State).

-endif.

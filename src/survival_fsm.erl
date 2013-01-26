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
		 send_display_map/1, send_done/1, get_client_state/1]).

%% gen_fsm callbacks
-export([init/1, choose_direction/2, choose_direction/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
		 ranged_combat/3, melee_combat/3]).

-record(state, {map, player, combat={}, day, time, scenario, options}).

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

send_done(FSM) ->
	io:format("Sending done event to FSM~n"),
    gen_fsm:sync_send_event(FSM, {done}).

send_display_map(FSM) ->
	io:format("Sending map request to FSM~n"),
    gen_fsm:sync_send_all_state_event(FSM, {display_map}).

send_display_status(FSM) ->
	io:format("Sending status request to FSM~n"),
    gen_fsm:sync_send_all_state_event(FSM, {display_status}).

send_display_legend(FSM) ->
	io:format("Sending legend request to FSM~n"),
    gen_fsm:sync_send_all_state_event(FSM, {display_legend}).

get_client_state(FSM) ->
	io:format("Sending client state request to FSM~n"),
	gen_fsm:sync_send_all_state_event(FSM, {client_state}).

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
	FirstState = basic_scenario_state(Player, Map),
	notice(FirstState, "Initial FSM State created: ~p~n", [printable_state(FirstState)]),
	display_map(FirstState),
	display_status(FirstState),
	display_legend(FirstState),
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
choose_direction({done}, _From, StateData) ->
	% player has finished moving for the day
	case StateData#state.time of
		am ->
			% update MP, switch to PM
			NewPlayer = StateData#state.player#player{mp=?MAX_MP},
			NewStateData = StateData#state{player = NewPlayer, time=pm},
			display_map(NewStateData),
			display_status(NewStateData),
			{reply, ok, choose_direction, NewStateData};
		pm ->
			% update MP, switch back to am and up the day count
			NewPlayer = StateData#state.player#player{mp=?MAX_MP},
			NewDay = StateData#state.day + 1,
			NewStateData = StateData#state{player=NewPlayer, time=am, day= NewDay},
			if 
				NewDay > ?DAYS_OF_FOOD ->
					io:format("~n~nYou have run out of food and died!~nFinal Status:~n~n"),
					display_status(NewStateData),
					{stop, normal, lost_game, NewStateData};
				true ->
					display_map(NewStateData),
					display_status(NewStateData),
					{reply, ok, choose_direction, NewStateData}
			end
	end;
choose_direction({direction, Direction}, _From, StateData) 
    when Direction < 1; Direction > 6 ->
	% bad direction input
	{reply, {invalid_move, invalid_direction}, choose_direction, StateData};
choose_direction({direction, Direction}, _From, 
				 StateData = #state{map=Map = #smap{stationloc = Stationloc}, player=Player}) ->
    % io:format("Applying ~b direction to player location~n", [Direction]),
	% apply the direction and handle result
	MoveResult = survival_map:apply_move(Map, Player, Direction),
	case MoveResult of
		{invalid_move, Reason} ->
			io:format("Move invalid because: ~p~n", [Reason]),
			display_map(StateData),
			display_status(StateData),
			{reply, {invalid_move, Reason}, choose_direction, StateData};
		{valid, AppliedPlayer = #player{loc = Stationloc}} ->  % test for win condition
			% Player's new location is the station, he wins!
			io:format("~n~nCongratulations! You have reached the station!~nFinal Status:~n~n"),
			display_status(StateData#state{player=AppliedPlayer}),
			{stop, normal, won_game, StateData#state{player=AppliedPlayer}};
		{valid, AppliedPlayer} ->
	    	%else, continue with the turn
			Terrain = survival_map:get_terrain_at_loc(AppliedPlayer#player.loc, StateData#state.map),
			% io:format("Received: ~w from terrain at loc~n", [Terrain]),
			AnimalRoll = survival_combat:animal_roll(Terrain),
			% io:format("Received: ~w from combat roll~n", [AnimalRoll]),
			case AnimalRoll of
				{no_animal} ->
					% TODO auto-select {done} if no mp or no where to move to
			    	UpdatedStateData = StateData#state{player = AppliedPlayer},
					display_map(UpdatedStateData),
			    	display_status(UpdatedStateData),
			    	Reply = ok,
			        {reply, Reply, choose_direction, UpdatedStateData};
				Animal ->
					true = is_record(Animal, monster),
			    	Reply = ok,
					HasRanged = survival_player:has_ranged(AppliedPlayer#player.weapons),
					case HasRanged of
						true ->
							UpdatedStateData = StateData#state{player = AppliedPlayer, 
								combat= {ranged, Animal}},
							display_map(UpdatedStateData),
					    	display_status(UpdatedStateData),
					        {reply, Reply, ranged_combat, UpdatedStateData};
						false ->
							UpdatedStateData = StateData#state{player = AppliedPlayer, 
								combat= {melee, Animal}},
							display_map(UpdatedStateData),
					    	display_status(UpdatedStateData),
							{reply, Reply, melee_combat, UpdatedStateData}
					end
			end
    end;
choose_direction(Event, _From, StateData) ->
	unexpected(Event, choose_direction),
    Reply = ok,
    {reply, Reply, choose_direction, StateData}.


%% --------------------------------------------------------------------
%% Func: ranged_combat/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
ranged_combat({done}, _From, 
			  #state{combat={ranged, Animal}} = StateData) ->
	% continue to melee
	Reply = ok,
	{reply, Reply, melee_combat, StateData#state{combat={melee, Animal}} };
ranged_combat({weapon, Choice}, _From, 
			  #state{combat={ranged, Animal}, player=Player} = StateData) when is_integer(Choice)->
	WeaponList = Player#player.weapons,
	Valid = survival_weapons:is_valid_weapon(ranged, WeaponList, Choice),
	% if not a valid weapon choice return error
	if
		Valid ->
			% get weapon and fire it 
			{Ref, Weapon} = lists:nth(Choice, WeaponList),
			UpdatedWeaponList = lists:keyreplace(Ref, 1, WeaponList, {Ref, survival_weapons:fire_weapon(Weapon)}),
			% check attack success
			case survival_combat:player_attack(Weapon#weapon.range, Animal) of
				{success} ->
					io:format("You hit the ~s!~n", [Animal#monster.mname]),
					% check for animal death
					io:format("You killed the ~s!~n", [Animal#monster.mname]),
					% return to normal move state
					Combat = {},
					UpdatedStateData = StateData#state{combat=Combat, 
							         player=Player#player{weapons=UpdatedWeaponList} },
					display_status(UpdatedStateData),
					Reply = ok,
					NextState = choose_direction;
				{failure} ->
					io:format("You missed the ~s!~n", [Animal#monster.mname]),
					% update the weapon list and switch to melee combat
					Combat = {melee, Animal},
					UpdatedStateData = StateData#state{player=Player#player{weapons=UpdatedWeaponList},
													   combat=Combat },
					display_status(UpdatedStateData),
					% continue to melee
					Reply = ok,
					NextState = melee_combat
			end,
			{reply, Reply, NextState, UpdatedStateData};
		true ->
			% not a valid weapon choice, return error
			{reply, {invalid_move, invalid_weapon_choice}, ranged_combat, StateData}
	end;
ranged_combat(Event, _From, StateData) ->
	unexpected(Event, ranged_combat),
    Reply = ok,
    {reply, Reply, ranged_combat, StateData}.



%% --------------------------------------------------------------------
%% Func: melee_combat/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
melee_combat({weapon, Choice}, _From, 
			 StateData = #state{player=Player, combat={melee, Animal}}) 
  when is_integer(Choice) ->
	% TODO melee combat round
	WeaponList = Player#player.weapons,
	Valid = survival_weapons:is_valid_weapon(melee, (StateData#state.player)#player.weapons, Choice),
	if
		Valid ->
			% get weapon and fire it 
			{Ref, Weapon} = lists:nth(Choice, WeaponList),
			UpdatedWeaponList = lists:keyreplace(Ref, 1, WeaponList, {Ref, survival_weapons:fire_weapon(Weapon)}),
			% check attack success
			case survival_combat:player_attack(Weapon#weapon.melee, Animal) of
				{success} ->
					io:format("You hit the ~s!~n", [Animal#monster.mname]),
					% check for animal death
					io:format("You killed the ~s!~n", [Animal#monster.mname]),
					% return to normal move state
					Combat = {},
					UpdatedStateData = StateData#state{combat=Combat, 
							         player=Player#player{weapons=UpdatedWeaponList} },
					display_status(UpdatedStateData),
					Reply = ok,
					{reply, Reply, choose_direction, UpdatedStateData};
				{failure} ->
					io:format("You missed the ~s!~n", [Animal#monster.mname]),
					
					% roll for animal attack vs. player
					case survival_combat:animal_attack(Animal) of
						{success} ->
							io:format("The ~s hit you!~n", [Animal#monster.mname]),
							% subtrack a wound point
							CurrentWS = Player#player.ws,
							WoundedPlayer = Player#player{ws=CurrentWS - 1},
							% check for player death
							case WoundedPlayer#player.ws of
								0 ->
									io:format("You have been killed by the ~s!~nFinalStatus:~n~n", [Animal#monster.mname]),
									UpdatedStateData = StateData#state{player=WoundedPlayer#player{weapons=UpdatedWeaponList} },
									display_status(UpdatedStateData),
									{stop, normal, lost_game, UpdatedStateData};
								_Else ->
									% on to the next melee round
									% update the weapon list and player
									UpdatedStateData = StateData#state{player=Player#player{weapons=UpdatedWeaponList},
																	   combat={melee, Animal} },
									display_status(UpdatedStateData),
									% continue to melee
									Reply = ok,
									{reply, Reply, melee_combat, UpdatedStateData}
							end;
						{failure} ->
							io:format("The ~s missed you!~n", [Animal#monster.mname]),		

							% update the weapon list and player
							UpdatedStateData = StateData#state{player=Player#player{weapons=UpdatedWeaponList},
															   combat={melee, Animal} },
							display_status(UpdatedStateData),
							% continue to melee
							Reply = ok,
							{reply, Reply, melee_combat, UpdatedStateData}
					end
			end;
		true ->
			% not a valid weapon choice, return error
			{reply, {invalid_move, invalid_weapon_choice}, ranged_combat, StateData}
	end;
melee_combat(Event, _From, StateData) ->
	unexpected(Event, melee_combat),
    Reply = ok,
    {reply, Reply, melee_combat, StateData}.
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
handle_sync_event({client_state}, _From, StateName, StateData) ->
	% return the client viewable state data for display/info
    Reply = {ok, StateData},
    {reply, Reply, StateName, StateData};
handle_sync_event({display_status}, _From, StateName, StateData) ->
	ok = display_status(StateData),
    Reply = ok,
    {reply, Reply, StateName, StateData};
handle_sync_event({display_legend}, _From, StateName, StateData) ->
	display_legend(StateData),
    Reply = ok,
    {reply, Reply, StateName, StateData};
handle_sync_event({display_map}, _From, StateName, StateData) ->
	display_map(StateData),
    Reply = ok,
    {reply, Reply, StateName, StateData};
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
  StartingLoc = lists:nth(random:uniform(length(Map#smap.starts)), Map#smap.starts),
  % new player record
  Player = survival_player:new_player(PlayerName, StartingLoc),
  % TODO replace with player weapon choices
  LoadedPlayer = survival_player:add_weapons(Player, survival_weapons:default_list()),
  
  [LoadedPlayer, Map].

basic_scenario_state(Player, Map) ->
	#state{player=Player, map=Map, combat={}, 
						day=1, time=am, scenario=basic, options=[]}.

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

display_status(#state{day=Day, time=Time, 
					  player=#player{pname=Name, weapons=Weapons, ws=Wounds, mp=MP},
					  combat=Combat}) ->
	io:format("Player: ~s~nDay:~b Time:~s MP:~b WS:~b~n", 
			  [Name, Day, string:to_upper(atom_to_list(Time)), MP, Wounds]),
	print_weapons(Weapons),
	case Combat of 
		{} ->
			% no combat to report
			%io:format("Not in combat~n"),
			ok;
		{ranged, #monster{mname=Animal}} ->
			io:format("Ranged Combat Round against ~s~nSelect a ranged weapon or select \"done\".~n", [Animal]);
		{melee, #monster{mname=Animal}} ->
			io:format("Melee Combat Round against ~s~nSelect a melee weapon or select \"done\".~n", [Animal])
	end,
	io:format("~n"),
	ok.

print_weapons(Weapons) ->
	io:format("Weapons:~n"),
	print_weapon(Weapons, 1),
	ok.	

print_weapon([], _Index) -> ok; % base case
print_weapon([{_Ref, Weap}|Rest], Index) ->
	%print one weapon at a time
	if 
		is_integer(Weap#weapon.maxrounds) ->
			io:format("~2b. ~-15s Range Pow:~2w Melee Pow:~2w Rounds:    ~2b/~2b~n", 
					   [Index,
						Weap#weapon.displayname,
						Weap#weapon.range,
						Weap#weapon.melee, 
						Weap#weapon.rounds, Weap#weapon.maxrounds]);
		true ->
			io:format("~2b. ~-15s Range Pow:~2w Melee Pow:~2w Rounds:~s~n", 
					   [Index,
						Weap#weapon.displayname, 
						Weap#weapon.range,
						Weap#weapon.melee, 
						Weap#weapon.rounds])
	end,
	%print the rest
	print_weapon(Rest, Index + 1).

%% --------------------------------------------------------------------
%% eunit tests
%% --------------------------------------------------------------------
-ifdef(TEST).
test_start() ->
	{ok, Game} = survival_fsm:start("Survival FSM"),
	Game.

test_end(Game) ->
	ok = survival_fsm:quit(Game).

basic_scenario_test_state() ->
	% reset RNG to get a roll of 4 on d6
	random:seed(1, 1, 1000),
	Player = survival_player:new_player(),
	LoadedPlayer = survival_player:add_weapons(Player, survival_weapons:default_list()),
    Map = survival_map:default_map(),
	basic_scenario_state(LoadedPlayer, Map).

start_end_test() ->
	FSM = test_start(),
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
    State = #state{map=Map, player=Player, 
	  day=1, time=am, scenario=basic, options=[]},

    ok = display_status(State).

get_client_state_test() ->
	Game = test_start(),
	{ok, State} = get_client_state(Game),
	true = is_record(State, state),
	"Survival FSM" = State#state.player#player.pname,
	1 = State#state.day,
	am = State#state.time,
	test_end(Game).

print_weapons_test() ->
	% create some weapon lists
	AllWeapons = [{make_ref(), survival_weapons:new_weapon(WeaponAtom)} || WeaponAtom <- ?WEAPON_LIST],
	print_weapons(AllWeapons),
	OneWeapon = [{make_ref(), survival_weapons:new_weapon(hands)}],
	print_weapons(OneWeapon).

invalid_direction_test() ->
    Player = survival_player:new_player(),
    Map = survival_map:default_map(),
    State = #state{map=Map, player=Player},

    ?assertEqual({reply, {invalid_move, invalid_direction}, choose_direction, State},
      choose_direction({direction, 0}, self(), State)),
    ?assertEqual({reply, {invalid_move, invalid_direction}, choose_direction, State},
      choose_direction({direction, 7}, self(), State)).

win_condition_test() ->
    Player = survival_player:new_player(),
    PlayerNextToStation = Player#player{loc={10, 18}},
    Map = survival_map:default_map(),
    State = #state{map=Map, player=PlayerNextToStation, combat={},
	day=1, time=am, scenario=basic, options=[]},
    Result = choose_direction({direction, 3}, self(), 
				 State),
    {stop, normal, won_game, StateData} = Result,
	io:format("FSM MP before call ~b, MPofStation:~b, MPAfterCall ~b Terrain ~p~n", 
			  [PlayerNextToStation#player.mp,
			   survival_map:get_mp(station),
			   StateData#state.player#player.mp,
			   survival_map:get_terrain_at_loc({11, 18}, Map)]),
    ?assertEqual(StateData#state.player, 
				 PlayerNextToStation#player{loc=Map#smap.stationloc, 
											mp=PlayerNextToStation#player.mp - survival_map:get_mp(station)}),
    ok.

client_state_test() ->
	Game = test_start(),
	
	{ok, State} = get_client_state(Game),
	?assert(is_record(State, state)),
	
	
	test_end(Game).
	
done_moving_test() ->
	StateData = basic_scenario_test_state(),
	NewStateData = test_regular_done(StateData, 11),
	
	% 12th done we should run out of food in basic game
	{stop, normal, lost_game, _LostStateData} = 
		choose_direction({done}, self(), NewStateData).

animal_roll_forest_jalait_ranged_test() ->
	StateData = basic_scenario_test_state(),
	
	% reset RNG to get a roll of 4 on d6
	random:seed(1, 1, 1000),
	
	Result = choose_direction({direction, 3}, self(), StateData),
    {reply, ok, ranged_combat, UpdatedStateData} = Result,
	{Range, #monster{atom=Animal}} = UpdatedStateData#state.combat,
	?assertEqual(ranged, Range),
	?assertEqual(jalait, Animal),
	ok.

animal_roll_forest_jalait_skip_ranged_test() ->
	% create player with no ranged weapons
	Player = survival_player:new_player(),
	
	InitialData = basic_scenario_test_state(),
	StateData = InitialData#state{player=Player},
	
	% reset RNG to get a roll of 4 on d6
	random:seed(1, 1, 1000),
	
	Result = choose_direction({direction, 3}, self(), StateData),
    {reply, ok, melee_combat, UpdatedStateData} = Result,
	{Range, #monster{atom=Animal}} = UpdatedStateData#state.combat,
	?assertEqual(melee, Range),
	?assertEqual(jalait, Animal),
	ok.

no_animal_roll_test() ->
	StateData = basic_scenario_test_state(),
	
	% reset RNG to get a roll of 1 on d6
	random:seed(1, 1, 1),
	
	Result = choose_direction({direction, 3}, self(), StateData),
    {reply, ok, choose_direction, UpdatedStateData} = Result,
	?assertEqual({}, UpdatedStateData#state.combat),
	ok.

ranged_combat_hit_test() ->
	StateData = basic_scenario_test_state(),
	
	% add ranged combat to state
	% corydal will be hit on a roll of 1
	CombatStateData = StateData#state{combat={ranged, #monster{atom=corydal, mname="Corydal", category=1}}},
	
	% reset RNG to get a roll of 1 on d6
	random:seed(1, 1, 1),
	
	Result = ranged_combat({weapon, 2}, self(), CombatStateData),
    {reply, ok, choose_direction, UpdatedStateData} = Result,
	?assertEqual({}, UpdatedStateData#state.combat),
	ok.
	
ranged_combat_miss_test() ->
	StateData = basic_scenario_test_state(),
	
	% add ranged combat to state
	% maizar will be missed on a roll of 1
	CombatStateData = StateData#state{combat={ranged, #monster{atom=maizar, mname="Maizar", category=4}}},
	
	% reset RNG to get a roll of 4 on d6
	random:seed(1, 1, 1000),
	
	Result = ranged_combat({weapon, 2}, self(), CombatStateData),
    {reply, ok, melee_combat, UpdatedStateData} = Result,
	?assertEqual({melee, #monster{atom=maizar, mname="Maizar", category=4}}, UpdatedStateData#state.combat),
	ok.

melee_combat_hit_test() ->
	StateData = basic_scenario_test_state(),
	
	% add melee combat to state
	% maizar will be hit on a roll of 1
	CombatStateData = StateData#state{combat={melee, #monster{atom=maizar, mname="Maizar", category=4, attack=3}}},
	
	% reset RNG to get a roll of 1 on d6
	random:seed(1, 1, 1),
	
	print_weapons(CombatStateData#state.player#player.weapons),
	
	Result = melee_combat({weapon, 4}, self(), CombatStateData),
    {reply, ok, choose_direction, UpdatedStateData} = Result,
	?assertEqual({}, UpdatedStateData#state.combat),
	ok.

melee_combat_miss_and_be_missed_test() ->
	StateData = basic_scenario_test_state(),
	
	% add melee combat to state
	% attack=0 will always miss
	CombatStateData = StateData#state{combat={melee, #monster{atom=maizar, mname="Maizar", category=4, attack=0}}},
	
	% reset RNG to get a roll of 4 on d6
	random:seed(1, 1, 1000),
	
	Result = melee_combat({weapon, 1}, self(), CombatStateData),
    {reply, ok, melee_combat, UpdatedStateData} = Result,
	{Range, Animal} = UpdatedStateData#state.combat,
	?assertEqual(melee, Range),
	?assertEqual(#monster{atom=maizar, mname="Maizar", category=4, attack=0}, Animal),
	ok.

% test repeatedly selecting done for movement
test_regular_done(StateData, 0) ->
	StateData;
test_regular_done(StateData, N) ->
	{reply, ok, choose_direction, NewStateData} = 
		choose_direction({done}, self(), StateData),
	case StateData#state.time of
		am ->
			?assertEqual(pm, NewStateData#state.time),
			?assertEqual(StateData#state.day, NewStateData#state.day);
		pm ->
			?assertEqual(am, NewStateData#state.time),
			?assertEqual(StateData#state.day + 1, NewStateData#state.day)
	end,
									 
	test_regular_done(NewStateData, N - 1).
	
 
-endif.

%%%---------------------------------------------------------------------
%%% Description module player
%%%---------------------------------------------------------------------
%%% Contains the player info methods
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% new_player()
%%%   creates a new player record
%%%---------------------------------------------------------------------


-module(survival_player).
-author("Brian E. Williams").

-compile([debug_info]).

-export([new_player/0, new_player/2, add_weapon/2, remove_weapon/2, total_weight/1]).

-include("survival.hrl").

new_player() ->
	new_player("Test Player", {1,1}).
new_player(Name, Start) when is_list(Name), is_tuple(Start)->
    #player{pname=Name, loc=Start, weapons = [{make_ref(), survival_weapons:new_weapon(hands)}]}.

add_weapon(Player = #player{weapons = Weapons}, Weapon = #weapon{atom = Atom, maxrounds = MaxRounds}) 
  when is_record(Player, player), is_record(Weapon, weapon), Atom /= hands ->
	FullWeapon = Weapon#weapon{rounds = MaxRounds},
	Ref = make_ref(),
	{Player#player{weapons = Weapons ++ [{Ref, FullWeapon}]}, Ref}.

remove_weapon(Player = #player{weapons = Weapons}, Ref) 
  when is_record(Player, player), is_reference(Ref) ->
	Player#player{weapons = lists:keydelete(Ref, 1, Weapons)}.

total_weight(#player{weapons = Weapons}) ->
	lists:sum([Weight || {_Ref, #weapon{weight=Weight}} <- Weapons]).


-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").

new_player_test() ->
	Player = new_player("EUnit Test Player", {5, 6}),
	?assertEqual( {5, 6}, Player#player.loc),
	?assertEqual( "EUnit Test Player", Player#player.pname),
	?assertEqual(?MAX_WOUNDS, Player#player.ws),
	?assertEqual(1, length(Player#player.weapons)),
	ok.

add_remove_weapon_test() ->
	Player = new_player("EUnit Test Player", {5, 6}),
	{UpdatedPlayer, Ref} = add_weapon(Player, survival_weapons:new_weapon(spear)),
	?assertEqual(2, length(UpdatedPlayer#player.weapons)),
	?assertEqual({Ref, survival_weapons:new_weapon(spear)}, lists:nth(2, UpdatedPlayer#player.weapons)),
	RemovedPlayer = remove_weapon(UpdatedPlayer, Ref),
	?assertEqual(1, length(RemovedPlayer#player.weapons)),
	ok.

total_weight_test() ->
	Player = new_player("EUnit Test Player", {5, 6}),
	?assertEqual(0, total_weight(Player)),
	LoadedPlayer = lists:foldl(
	  fun(Weapon, Acc) -> 
			  {NewPlayer, _Ref} = add_weapon(Acc, survival_weapons:new_weapon(Weapon)),
			  NewPlayer end, 
      Player, [spear, auto_pistol, lightsword]),
    ?assertEqual(4, total_weight(LoadedPlayer)).
-endif.
	


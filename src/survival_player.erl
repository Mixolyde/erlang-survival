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

-export([new_player/0, new_player/2, add_weapon/2, add_weapons/2, remove_weapon/2, total_weight/1,
		 has_ranged/1]).

-include("survival.hrl").

new_player() ->
	new_player("Test Player", {1,1}).
new_player(Name, Start) when is_list(Name), is_tuple(Start)->
    #player{pname=Name, loc=Start, weapons = [{make_ref(), survival_weapons:new_weapon(hands)}]}.

add_weapon(Player = #player{weapons = Weapons}, Weapon) 
  when is_record(Player, player), is_atom(Weapon), Weapon /= hands ->
	FullWeapon = survival_weapons:new_weapon(Weapon),
	AddedWeight = total_weight(Weapons) + survival_weapons:get_weight(Weapon), 
	if
		AddedWeight > ?MAX_WEIGHT ->
			{error, max_weight_exceeded};
		true ->		
			Ref = make_ref(),
			{Player#player{weapons = Weapons ++ [{Ref, FullWeapon}]}, Ref}
	end.

add_weapons(Player, List) when is_record(Player, player), is_list(List) ->
	AddedWeight = total_weight(Player#player.weapons) + 
					  lists:sum([survival_weapons:get_weight(Weapon) || Weapon <- List]),
	if
		AddedWeight > ?MAX_WEIGHT ->
			{error, max_weight_exceeded};
		true ->		
			UpdatedPlayer = lists:foldr(
			  fun(Weapon, AccPlayer) -> 
					  {PlayerWithWeapon, _Ref} = add_weapon(AccPlayer, Weapon),
					  PlayerWithWeapon end, Player, List),
			UpdatedPlayer
	end.

remove_weapon(Player = #player{weapons = Weapons}, Ref) 
  when is_record(Player, player), is_reference(Ref) ->
	Player#player{weapons = lists:keydelete(Ref, 1, Weapons)}.

total_weight(PlayerWeapons) ->
	lists:sum([Weight || {_Ref, #weapon{weight=Weight}} <- PlayerWeapons]).

has_ranged(PlayerWeapons) ->
	lists:any(fun({_Ref, #weapon{range=Range}}) ->
					  if
						  is_integer(Range) ->
							  true;
						  true ->
							  false
					  end
			  end, PlayerWeapons).


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
	{UpdatedPlayer, Ref} = add_weapon(Player, spear),
	?assertEqual(2, length(UpdatedPlayer#player.weapons)),
	?assertEqual({Ref, survival_weapons:new_weapon(spear)}, lists:nth(2, UpdatedPlayer#player.weapons)),
	RemovedPlayer = remove_weapon(UpdatedPlayer, Ref),
	?assertEqual(1, length(RemovedPlayer#player.weapons)),
	ok.

overweight_test() ->
	Player = new_player("EUnit Test Player", {5, 6}),
	FullPlayer = add_weapons(Player, [laser_carbine, laser_carbine]),
	?assertEqual({error, max_weight_exceeded}, add_weapon(FullPlayer, spear)),
	?assertEqual({error, max_weight_exceeded}, 
				 add_weapons(FullPlayer, [spear, laser_carbine, laser_carbine])).

add_weapons_test() ->
	Player = new_player("EUnit Test Player", {5, 6}),
	UpdatedPlayer = add_weapons(Player, [spear, auto_pistol, lightsword]),
	?assert(is_record(UpdatedPlayer, player)),
	?assertEqual(4, length(UpdatedPlayer#player.weapons)).

total_weight_test() ->
	Player = new_player("EUnit Test Player", {5, 6}),
	?assertEqual(0, total_weight(Player#player.weapons)),
	LoadedPlayer = add_weapons(Player, [spear, auto_pistol, lightsword]),
    ?assertEqual(4, total_weight(LoadedPlayer#player.weapons)).

has_ranged_test() ->
	List1 = [],
	?assertNot(has_ranged(List1)),
	Spear = {make_ref(), survival_weapons:new_weapon(spear)},
	LS = {make_ref(), survival_weapons:new_weapon(lightsword)},
	Hands = {make_ref(), survival_weapons:new_weapon(hands)},
	List2= [Spear, LS, Hands],
	?assertNot(has_ranged(List2)),
	LC = {make_ref(), survival_weapons:new_weapon(laser_carbine)},
	AP = {make_ref(), survival_weapons:new_weapon(auto_pistol)},
	List3 = [LC, AP],
	?assert(has_ranged(List3)),
	?assert(has_ranged(List2 ++ List3)).
	
	
-endif.
	


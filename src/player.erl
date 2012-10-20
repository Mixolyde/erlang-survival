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


-module(player).
-author("Brian E. Williams").

-compile([debug_info]).

-export([new_player/0, new_player/2, add_weapon/2, remove_weapon/2]).

-include("survival.hrl").

new_player() ->
	new_player("Test Player", {1,1}).
new_player(Name, Start) when is_list(Name), is_tuple(Start)->
    #player{pname=Name, loc=Start}.

add_weapon(Player = #player{weapons = Weapons}, Weapon = #weapon{maxrounds = MaxRounds}) 
  when is_record(Player, player), is_record(Weapon, weapon) ->
	FullWeapon = Weapon#weapon{rounds = MaxRounds},
	Player#player{weapons = Weapons ++ [FullWeapon]}.

remove_weapon(Player = #player{weapons = Weapons}, Weapon) 
  when is_record(Player, player), is_record(Weapon, weapon) ->
	Player#player{weapons = Weapons -- Weapon}.


-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").

new_player_test() ->
	Player = new_player("EUnit Test Player", {5, 6}),
	?assertEqual( #player{loc = {5, 6}, pname = "EUnit Test Player", weapons = [], ws = ?MAX_WOUNDS}, Player),
	?assertEqual([], Player#player.weapons),
	ok.

add_weapon_test() ->
	Player = new_player("EUnit Test Player", {5, 6}),
	UpdatedPlayer = add_weapon(Player, weapons:new_weapon(spear)),
	?assertEqual(1, length(UpdatedPlayer#player.weapons)),
	?assertEqual([weapons:new_weapon(spear)], UpdatedPlayer#player.weapons),
	
	ok.
-endif.
	


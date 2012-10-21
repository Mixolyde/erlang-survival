%% Author: bwilliams
%% Created: Oct 17, 2012
%% Description: TODO: Add description to weapons
-module(weapons).

%%
%% Include files
%%
-include("survival.hrl").
-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
-endif.

%%
%% Exported Functions
%%
-export([new_weapon/1, all_weapons/0, purchase_list/0]).

%%
%% API Functions
%%

new_weapon(spear) ->
	#weapon{atom=spear, displayname="Spear", maxrounds=unlimited, rounds=unlimited, melee=3, range=na, weight=1};
new_weapon(laser_carbine) ->
	#weapon{atom=laser_carbine, displayname="Laser Carbine", weight=4, melee=na, range=5, maxrounds=7, rounds=7};
new_weapon(auto_pistol) ->
	#weapon{atom=auto_pistol, displayname="Auto Pistol", weight=1, melee=4, range=2, maxrounds=10, rounds=10};
new_weapon(lightsword) ->
	#weapon{atom=lightsword, displayname="Lightsword", weight=2, melee=5, range=na, maxrounds=6, rounds=6};
new_weapon(rifle) ->
	#weapon{atom=rifle, displayname="Rifle", weight=3, melee=na, range=4, maxrounds=8, rounds=8};
new_weapon(grenade_launcher) ->
	#weapon{atom=grenade_launcher, displayname="Grenade Launcher", weight=5, melee=na, range=8, maxrounds=5, rounds=5};
new_weapon(energy_blaster) ->
	#weapon{atom=energy_blaster, displayname="Energy Blaster", weight=6, melee=na, range=9, maxrounds=3, rounds=3};
new_weapon(hands) ->
	#weapon{atom=hands, displayname="Hands", maxrounds=unlimited, rounds=unlimited, melee=0, range=na, weight=0};
new_weapon(_Any) ->
	error({unknown_weapon_type, _Any}).

%[hands, spear, laser_carbine, auto_pistol, 
%lightsword, rifle, grenade_launcher, energy_blaster]
all_weapons() -> ?WEAPON_LIST.
purchase_list() -> all_weapons() -- [hands].

%%
%% Local Functions
%%



%% --------------------------------------------------------------------
%%% Eunit test functions
%% --------------------------------------------------------------------
-ifdef(TEST).
purchase_list_test() ->
	?assertEqual(length(all_weapons()) - 1, length(purchase_list())).

new_weapons_test() ->
	Spear = new_weapon(spear),
	?assert(erlang:is_record(Spear, weapon)),
			
	Records = [new_weapon(Weapon) || Weapon <- ?WEAPON_LIST],
	?assertEqual(length(all_weapons()), length(Records)),
    ok.
-endif.
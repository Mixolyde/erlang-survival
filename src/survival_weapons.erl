%% Author: bwilliams
%% Created: Oct 17, 2012
%% Description: TODO: Add description to weapons
-module(survival_weapons).

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
-export([new_weapon/1, get_weight/1, all_weapons/0, purchase_list/0, default_list/0,
         is_range/1, is_melee/1, is_valid_weapon/3]).

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

get_weight(Weapon) ->
	WeaponRecord = new_weapon(Weapon),
	WeaponRecord#weapon.weight.

is_range(#weapon{range = Str}) -> is_integer(Str).

is_melee(#weapon{melee = Str}) -> is_integer(Str).


%[hands, spear, laser_carbine, auto_pistol, 
%lightsword, rifle, grenade_launcher, energy_blaster]
all_weapons() -> ?WEAPON_LIST.
purchase_list() -> all_weapons() -- [hands].
default_list() -> [spear, lightsword, auto_pistol, rifle].

is_valid_weapon(Type, Weapons, Choice) when Choice > 0, Choice =< length(Weapons) ->
	{_Ref, Weapon} = lists:nth(Choice, Weapons),
    is_valid_weapon(Type, Weapon);
is_valid_weapon(_Any, _Weapons, _Choice) -> false.

is_valid_weapon(melee, Weapon = #weapon{rounds=Rounds}) ->
	IsMelee = is_melee(Weapon),
    if
		IsMelee, is_integer(Rounds) andalso Rounds > 0 ->
			true;
		IsMelee, Rounds == unlimited ->
			true;
        true ->
            %all other weapons are invalid
			false
	end;
is_valid_weapon(ranged, Weapon = #weapon{rounds=Rounds}) ->
	IsRanged = is_range(Weapon),
    if
		IsRanged, is_integer(Rounds) andalso Rounds > 0 ->
			true;
		IsRanged, Rounds == unlimited ->
			true;
        true ->
            %all other weapons are invalid
			false
	end.


%%
%% Local Functions
%%



%% --------------------------------------------------------------------
%%% Eunit test functions
%% --------------------------------------------------------------------
-ifdef(TEST).
default_list_data() ->
	[{make_ref(), survival_weapons:new_weapon(Weapon)} || Weapon <- default_list()].

default_list_data_test() ->
	?assertEqual(length(default_list()), length(default_list_data())).

purchase_list_test() ->
	?assertEqual(length(all_weapons()) - 1, length(purchase_list())).

new_weapons_test() ->
	Spear = new_weapon(spear),
	?assert(erlang:is_record(Spear, weapon)),
			
	Records = [new_weapon(Weapon) || Weapon <- ?WEAPON_LIST],
	?assertEqual(length(all_weapons()), length(Records)),
    ok.

bad_weapon_test() ->
	?assertError({unknown_weapon_type, _Any}, new_weapon(junk)).

range_and_melee_test() ->
    ?assert(is_range(new_weapon(auto_pistol))),
	?assert(is_range(new_weapon(laser_carbine))),
	?assert(is_range(new_weapon(rifle))),
	?assert(is_melee(new_weapon(auto_pistol))),
	?assert(is_melee(new_weapon(hands))),
	?assert(is_melee(new_weapon(lightsword))),
	?assertNot(is_melee(new_weapon(laser_carbine))),
	?assertNot(is_melee(new_weapon(rifle))),
	?assertNot(is_range(new_weapon(hands))),
	?assertNot(is_range(new_weapon(lightsword))).
	

is_valid_weapon_list_size_test() ->
	?assertNot(is_valid_weapon(melee, [], -1)),
	?assertNot(is_valid_weapon(melee, [], 0)),
	?assertNot(is_valid_weapon(melee, [], 1)),
	?assertNot(is_valid_weapon(melee, default_list_data(), length(default_list_data()) + 1)),
	?assertNot(is_valid_weapon(ranged, [], -1)),
	?assertNot(is_valid_weapon(ranged, [], 0)),
	?assertNot(is_valid_weapon(ranged, [], 1)),
	?assertNot(is_valid_weapon(ranged, default_list_data(), 0)),
	?assertNot(is_valid_weapon(ranged, default_list_data(), length(default_list_data()) + 1)).

is_valid_melee_weapon_test() ->
	?assert(is_valid_weapon(melee, default_list_data(), 1)),
	?assert(is_valid_weapon(melee, default_list_data(), 2)),
	?assert(is_valid_weapon(melee, default_list_data(), 3)),
	?assertNot(is_valid_weapon(melee, default_list_data(), 4)).

is_valid_ranged_weapon_test() ->
	?assertNot(is_valid_weapon(ranged, default_list_data(), 1)),
	?assertNot(is_valid_weapon(ranged, default_list_data(), 2)),
	?assert(is_valid_weapon(ranged, default_list_data(), 3)),
	?assert(is_valid_weapon(ranged, default_list_data(), 4)).

is_valid_ranged_weapon_ammo_test() ->
	FullWeapon = survival_weapons:new_weapon(rifle),
	EmptyWeapon = FullWeapon#weapon{rounds = 0},
	
	?assertNot(is_valid_weapon(ranged, [{make_ref(), EmptyWeapon}], 1)).

is_valid_melee_weapon_ammo_test() ->
	FullWeapon = survival_weapons:new_weapon(lightsword),
	EmptyWeapon = FullWeapon#weapon{rounds = 0},
	?assertNot(is_valid_weapon(melee, [{make_ref(), EmptyWeapon}], 1)).

-endif.
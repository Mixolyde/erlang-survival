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
	#weapon{displayname="Spear", maxrounds=unlimited, rounds=unlimited, melee=3, range=na, weight=1};
new_weapon(hands) ->
	#weapon{displayname="Hands", maxrounds=unlimited, rounds=unlimited, melee=0, range=na, weight=0};
new_weapon(_Any) ->
	error({unknown_weapon_type, _Any}).

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
	Weapon = new_weapon(spear),
	?assert(erlang:is_record(Weapon, weapon)),
			
	% Records = [new_weapon(Weapon) || Weapon <- ?WEAPON_LIST],
	% ?assertEqual(length(all_weapons()), length(Records)),
    ok.
-endif.
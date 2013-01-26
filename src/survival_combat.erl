%% Author: bwilliams
%% Created: Oct 18, 2012
%% Description: TODO: Add description to survival_combat
-module(survival_combat).

%%
%% Include files
%%
-include("survival.hrl").
-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
-endif.

% {{name, weapon effect category}, random roll letter}
-define(ANIMALS, [{#monster{atom=maizar, mname="Maizar", category=4, attack=3}, a}, 
				  {#monster{atom=teklek, mname="Teklek", category=2, attack=3}, b}, 
				  {#monster{atom=corydal, mname="Corydal", category=1, attack=2}, c}, 
				  {#monster{atom=genebrach, mname="Genebrach", category=3, attack=4}, d},
				  {#monster{atom=ceekal, mname="Ceekal", category=1, attack=3}, e}, 
				  {#monster{atom=jalait, mname="Jalait", category=3, attack=3}, f}, 
				  {#monster{atom=shenthe, mname="Shenthe", category=2, attack=4}, g}, 
				  {#monster{atom=zeget, mname="Zeget", category=2, attack=2}, h}]).

% weapon strengths against each category based on strength roles
% strength is the number or lower to roll on a d6 that hits
% 0, 2, 3, 4, 5
% strengths 8 and 9 always hit
-define(WEAPON_EFFECTS, [[1, 3, 4, 5, 6],
						 [0, 2, 3, 4, 5],
						 [0, 1, 2, 3, 4],
						 [0, 0, 1, 2, 4]]).
-define(TERRAIN_ANIMALS, 
    [{mountains, [a, b, c, d]},
     {hills,  [b, c, d, e]},
     {rough,  [c, e, f, g]},
     {forest, [d, f, g, h]},
     {clear,  [e, f, g, h]},
     {marsh,  [f, g, h, a]},
     {river,  [e, f, g, h]}]).

-define(TERRAIN_WITH_ANIMALS, 
    [mountains, hills, rough, forest, clear, marsh, river]).

%%
%% Exported Functions
%%
-export([animal_roll/1, player_attack/2, animal_attack/1]).

%%
%% API Functions
%%
animal_roll(Terrain) ->
	Member = lists:member(Terrain, ?TERRAIN_WITH_ANIMALS), 
	% io:format("Received: ~w from terrain is member call~n", [Member]),
	if 
		Member ->		
		    Roll = random:uniform(6),
			animal_roll(Terrain, Roll);
		true ->
			{no_animal}
	end.

animal_roll(Terrain, Roll) ->
	case Roll of
		1 ->
			{no_animal};
		2 ->
			{no_animal};
		Roll ->   % rolls 3, 4, 5, 6 index into the terrain animal list
			{Terrain, LetterList} = lists:keyfind(Terrain, 1, ?TERRAIN_ANIMALS),
			Letter = lists:nth(Roll - 2, LetterList),
			{Monster, Letter} = lists:keyfind(Letter, 2, ?ANIMALS),
			Monster
	end.

player_attack(Strength, _Animal) when is_integer(Strength) andalso Strength > 7 ->
	{success};
player_attack(0, Animal) ->
	%stupid hack because the weapon strengths are 0, 2, 3, 4, 5
	player_attack(1, Animal);
player_attack(Index, #monster{category=Cat}) when is_integer(Index) ->
	Roll = random:uniform(6),
	HitList = lists:nth(Cat, ?WEAPON_EFFECTS),
	Target = lists:nth(Index, HitList),
	io:format("Rolling for player attack combat. Target: ~b Roll:~b~n", [Target, Roll]),
	if 
		Roll =< Target ->
			{success};
		true ->
			{failure}
	end.

animal_attack(#monster{attack=Target}) ->
	Roll = random:uniform(6),
	io:format("Rolling for animal attack combat. Target: ~b Roll:~b~n", [Target, Roll]),
	if 
		Roll =< Target ->
			{success};
		true ->
			{failure}
	end.

%%
%% Local Functions
%%

-ifdef(TEST).
animal_roll_test() ->
	% test each terrain in the TERRAIN_WITH_ANIMALS list in the animal_roll function
	lists:map(
	  fun(Terrain) ->
		?assertEqual({no_animal}, animal_roll(Terrain, 1)),
		?assertEqual({no_animal}, animal_roll(Terrain, 2)),
		Animal3 = animal_roll(Terrain, 3),
		Animal4 = animal_roll(Terrain, 4),
		Animal5 = animal_roll(Terrain, 5),
		Animal6 = animal_roll(Terrain, 6),
		?assert(is_record(Animal3, monster)),
		?assert(is_record(Animal4, monster)),
		?assert(is_record(Animal5, monster)),
		?assert(is_record(Animal6, monster))
      end, 
	  ?TERRAIN_WITH_ANIMALS),
	% test always no animal rolled in these terrains
	?assertEqual({no_animal}, animal_roll(station)),
	?assertEqual({no_animal}, animal_roll(empty)),
	ok.

animal_attack_test() ->
	?assertEqual({success}, player_attack(8, #monster{category=1})),
	?assertEqual({success}, player_attack(9, #monster{category=1})),
	
	% reset RNG to get a roll of 4 on d6
	random:seed(1, 1, 1000),
	?assertEqual({success}, player_attack(3, #monster{category=1})),
	random:seed(1, 1, 1000),
	?assertEqual({failure}, player_attack(3, #monster{category=2})),
	% test 0 to 1 index conversion
	?assertEqual({failure}, player_attack(0, #monster{category=2})).

-endif.

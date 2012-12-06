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

-define(ANIMALS, [{maizar, a}, {teklek, b}, {corydal, c}, {genebrach, d},
					  {ceekal, e}, {jalait, f}, {shenthe, g}, {zeget, h}]).

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
-export([animal_roll/1]).

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
			{Atom, Letter} = lists:keyfind(Letter, 2, ?ANIMALS),
			{animal, Atom}
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
		{animal, Atom3} = animal_roll(Terrain, 3),
		{animal, Atom4} = animal_roll(Terrain, 4),
		{animal, Atom5} = animal_roll(Terrain, 5),
		{animal, Atom6} = animal_roll(Terrain, 6),
		?assert(is_atom(Atom3)),
		?assert(is_atom(Atom4)),
		?assert(is_atom(Atom5)),
		?assert(is_atom(Atom6))
      end, 
	  ?TERRAIN_WITH_ANIMALS),
	% test always no animal rolled in these terrains
	?assertEqual({no_animal}, animal_roll(station)),
	?assertEqual({no_animal}, animal_roll(empty)),
	ok.

-endif.

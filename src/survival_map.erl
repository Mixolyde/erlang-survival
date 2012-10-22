%%%---------------------------------------------------------------------
%%% Description module survival_map
%%%---------------------------------------------------------------------
%%% Creates a map for the game and provides methods for displaying and
%%% updating.
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%%---------------------------------------------------------------------

-module(survival_map).
-author("Brian E. Williams").

-compile([debug_info]).

-export([default_map/0, print_map_and_player/2, print_legend/0, get_mp/1, get_char/1, 
         get_terrain_at_loc/2, apply_move/3]).

-include("survival.hrl").
-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
-endif.

-define(TERRAIN_TYPES, [{forest, 2, $F}, {marsh, 3, $S}, {mountains, 4, $M},
				        {river, 4, $W}, {clear, 1, $C}, {hills, 3, $H}, {rough, 3, $R},
                        {station, 1, $*}, {empty, infinity, $\s}]).

get_mp(Terrain) when is_atom(Terrain) -> 
	{Terrain, MP, _Char} = lists:keyfind(Terrain, 1, ?TERRAIN_TYPES),
	MP.

get_char(player) -> 
	$@;
get_char(Terrain) when is_atom(Terrain) -> 
	{Terrain, _MP, Char} = lists:keyfind(Terrain, 1, ?TERRAIN_TYPES),
	Char.

get_terrain_at_loc({X, Y}, #smap{terrain=Terrain}) ->
	lists:nth(X, lists:nth(Y, Terrain)).

apply_move(#smap{terrain = _Terrain, size={_XSize, _YSize}}, 
    Player = #player{loc={XPlayer, YPlayer}, mp=_MP}, Direction) ->
    AppliedMove = apply_direction_math({XPlayer, YPlayer}, Direction),
    % TODO test move for validity

    % TODO return updated player with new loc and MP
    {valid, Player#player{loc=AppliedMove}}.

%% take an {X, Y} location on the map and return the new location
%% after direction (1-6), does not check for validity of new loc
%% requires some hexagon math because of the staggered rows
apply_direction_math({X, Y}, 1) when Y rem 2 == 1 ->
  {X - 1, Y - 1};
apply_direction_math({X, Y}, 1) when Y rem 2 == 0 ->
  {X, Y - 1};
apply_direction_math({X, Y}, 2) when Y rem 2 == 1 ->
  {X, Y - 1};
apply_direction_math({X, Y}, 2) when Y rem 2 == 0 ->
  {X + 1, Y - 1};
apply_direction_math({X, Y}, 3) ->
  {X + 1, Y};
apply_direction_math({X, Y}, 4) when Y rem 2 == 1 ->
  {X, Y + 1};
apply_direction_math({X, Y}, 4) when Y rem 2 == 0 ->
  {X + 1, Y + 1};
apply_direction_math({X, Y}, 5) when Y rem 2 == 1 ->
  {X - 1, Y + 1};
apply_direction_math({X, Y}, 5) when Y rem 2 == 0 ->
  {X, Y + 1};
apply_direction_math({X, Y}, 6) ->
  {X - 1, Y}.


print_legend() ->
	io:format("Directions:          Terrain:~n"),
	io:format("  1   2              ~c = Forest (~b MP)   ~c = Water     (~b MP)~n",
			 [survival_map:get_char(forest), survival_map:get_mp(forest), survival_map:get_char(river), survival_map:get_mp(river)]),
	io:format("   \\ /               ~c = Rough  (~b MP)   ~c = Mountains (~b MP)~n",
			 [survival_map:get_char(rough), survival_map:get_mp(rough), survival_map:get_char(mountains), survival_map:get_mp(mountains)]),
	io:format("6 - @ - 3            ~c = Hills  (~b MP)   ~c = Swamp     (~b MP)~n",
			 [survival_map:get_char(hills), survival_map:get_mp(hills), survival_map:get_char(marsh), survival_map:get_mp(marsh)]),
	io:format("   / \\               ~c = Clear  (~b MP)   ~c = Station   (~b MP)~n",
			 [survival_map:get_char(clear), survival_map:get_mp(clear), survival_map:get_char(station), survival_map:get_mp(station)]),
	io:format("  5   4              ~c = Player~n", [survival_map:get_char(player)]),
	ok.

default_map() -> #smap{size={20,18}, stationloc={10, 18}, 
    starts=[{1, 3}, {4, 1}, {8, 1}, {13, 1}, {17, 1}, {19, 4}], terrain=
    [ [forest, forest, forest, forest, forest, river, forest, rough, rough, rough, mountains, hills, hills,
       forest, forest, hills, hills, forest, forest, forest], % row 1
      [forest, rough, forest, hills, rough, river, marsh, rough, hills, mountains, mountains, hills, hills,
	   hills, hills, hills, forest, hills, forest], % row 2
	  [rough, rough, hills, hills, rough, river, rough, mountains, mountains, hills, mountains, mountains, hills,
	   mountains, mountains, rough, hills, hills, forest, forest], % row 3
	  [rough, rough, hills, mountains, rough, river, forest, mountains, mountains, mountains, mountains, mountains, mountains, mountains,
	   rough, rough, mountains, hills, hills], % row 4
      [rough, rough, mountains, mountains, mountains, hills, river, river] ++
		  dup(6, mountains) ++ [rough, rough, mountains, rough, river, river], % row 5
      [rough, hills] ++ dup(3, mountains) ++ [river, marsh] ++ dup(4, river) ++
		  dup(5, mountains) ++ [marsh, river, marsh], % row 6
      [forest] ++ dup(4, hills) ++ [river, forest, clear, rough, forest, forest, river, clear] ++
		  dup(4, mountains) ++ [marsh, river, forest], % row 7
      [hills, hills, forest, hills, river, forest, hills, hills, forest, rough, clear, river,
	   rough, rough, mountains, mountains, forest, river, forest], % row 8
	  [clear] ++ dup(4, river) ++ [marsh, rough] ++ dup(3, hills) ++ [mountains, forest, river, mountains, mountains] ++
		  dup(3, forest) ++ [river, forest], % row 9
      [river] ++ dup(3, forest) ++ [river, marsh, rough] ++ dup(4, mountains) ++ [clear] ++ dup(6, river) ++
		  [rough], % row 10
      [forest, forest, mountains, mountains, river, forest] ++ dup(5, mountains) ++ 
		  [rough, river, marsh, forest, marsh, marsh, rough, rough, rough], % row 11
      dup(4, empty) ++ [rough] ++ dup(3, mountains) ++ dup(3, hills) ++ [river, forest, forest] ++
		  dup(5, empty),  % row 12 length 19
	  dup(5, empty) ++ [rough, rough, hills, hills, rough, hills, river, hills] ++
		  dup(7, empty),  % row 13
	  dup(4, empty) ++ [rough] ++  dup(3, mountains) ++ [rough, forest, river, marsh, hills] ++
		  dup(6, empty), % row 14
	  dup(5, empty) ++ [rough, rough, mountains, rough, forest, river, forest, marsh] ++
		  dup(7, empty), % row 15
	  dup(8, empty) ++ [marsh, river, forest] ++
		  dup(8, empty),          % 16
      dup(8, empty) ++ [forest, forest, river, forest] ++
		  dup(8, empty), % 17
      dup(8, empty) ++ [forest, station, river] ++ dup(8, empty)       % last row 18
    ]}.

dup(Count, Terrain) -> lists:duplicate(Count, Terrain).

print_map_and_player(#smap{terrain=Terrain}, #player{loc=Loc}) ->
	print_map(Terrain, 1, Loc).

print_map([], _Line, _Loc) ->
    % io:format("Finished drawing."),
    ok;
print_map([First | Rest], Line, Loc) when Line rem 2 == 0->
    io:format(" "),    
    print_line_and_continue([First | Rest], Line, Loc);
print_map([First | Rest], Line, Loc) ->
    print_line_and_continue([First | Rest], Line, Loc).

print_line_and_continue([First | Rest], Line, Loc) ->
    print_line(First, Line, 1, Loc),
    print_map(Rest, Line+1, Loc).

print_line([], _Line, _CharIndex, _Loc) ->
    io:format("~n");
print_line([_First | Rest], Line, CharIndex, {CharIndex, Line}) ->
	% display player character if the X, Y match
    io:format("~c ", [get_char(player)]),
    print_line(Rest, Line, CharIndex + 1, {CharIndex, Line});
print_line([First | Rest], AnyLine, AnyIndex, {CharIndex, Line}) ->
    io:format("~c ", [get_char(First)]),
    print_line(Rest, AnyLine, AnyIndex + 1, {CharIndex, Line}).


%% --------------------------------------------------------------------
%%% Eunit test functions
%% --------------------------------------------------------------------
-ifdef(TEST).

-define(SMALL_MAP, [[forest, rough, mountains], [hills, river, marsh], [empty, station, clear]]).

get_loc_test() ->
	Map = #smap{terrain= ?SMALL_MAP},
	?assertEqual(forest,    get_terrain_at_loc({1, 1}, Map)),
	?assertEqual(rough,     get_terrain_at_loc({2, 1}, Map)),
	?assertEqual(mountains, get_terrain_at_loc({3, 1}, Map)),
	?assertEqual(hills,     get_terrain_at_loc({1, 2}, Map)),
	?assertEqual(river,     get_terrain_at_loc({2, 2}, Map)),
	?assertEqual(marsh,     get_terrain_at_loc({3, 2}, Map)),
	?assertEqual(empty,     get_terrain_at_loc({1, 3}, Map)),
	?assertEqual(station,   get_terrain_at_loc({2, 3}, Map)),
	?assertEqual(clear,     get_terrain_at_loc({3, 3}, Map)),
	
	ok.

line_test() ->
  print_line([forest, rough, marsh], 1, 1, {2,3}),
  io:format("Printed test line~n"),
  ok.
  
print_map_test() ->
  Twolines = [[forest, rough, marsh], [forest, rough, marsh]],
  print_map(Twolines, 1, {1, 1}),
  ok.

print_empty_map_test() ->
  print_map([], 1, {1, 2}),
  ok.

print_map_last_line_test() ->
  Oneline = [[forest, rough, marsh]],
  print_map(Oneline, 1, {3, 1}),
  ok.

map_line_length_test() ->
	Map = default_map(),
	LinesWithIndex = lists:zip(lists:seq(1, length(Map#smap.terrain)), Map#smap.terrain),

	[?assertEqual({Index, 20}, {Index, length(Line)}) 
	 || {Index, Line} <- LinesWithIndex, Index rem 2 == 1 ],
    [?assertEqual({Index, 19}, {Index, length(Line)}) 
	 || {Index, Line} <- LinesWithIndex, Index rem 2 == 0 ].
	
dup_test() ->
	[] = dup(0, stuff),
	[stuff] = dup(1, stuff),
	[stuff, stuff] = dup(2, stuff),
	[stuff, stuff, stuff] = dup(3, stuff),
	[stuff1, stuff1, stuff2, stuff2] = dup(2, stuff1) ++ dup(2, stuff2),
	ok.

get_mp_test() ->
	[?assert(get_mp(Terrain) == MP) || {Terrain, MP, _Char} <- ?TERRAIN_TYPES],
	ok.

get_char_test() ->
	[?assert(get_char(Terrain) == Char) || {Terrain, _MP, Char} <- ?TERRAIN_TYPES],
	ok.

apply_direction_math_test() ->
    ?assertEqual({3, 8}, apply_direction_math({4, 9}, 1)),
    ?assertEqual({4, 7}, apply_direction_math({4, 8}, 1)),
    ?assertEqual({4, 8}, apply_direction_math({4, 9}, 2)),
    ?assertEqual({5, 7}, apply_direction_math({4, 8}, 2)),
    ?assertEqual({5, 8}, apply_direction_math({4, 8}, 3)),
    ?assertEqual({4, 8}, apply_direction_math({4, 7}, 4)),
    ?assertEqual({5, 9}, apply_direction_math({4, 8}, 4)),
    ?assertEqual({3, 8}, apply_direction_math({4, 7}, 5)),
    ?assertEqual({4, 9}, apply_direction_math({4, 8}, 5)),
    ?assertEqual({3, 8}, apply_direction_math({4, 8}, 6)),
    ok.

-endif.

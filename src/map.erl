%%%---------------------------------------------------------------------
%%% Description module map
%%%---------------------------------------------------------------------
%%% Creates a map for the game and provides methods for displaying and
%%% updating.
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% default_map()
%%%   returns the default map
%%% print_map(Map)
%%%   prints the given map, map must be a list of lists of terrain
%%%   types 
%%%---------------------------------------------------------------------

-module(map).
-author("Brian E. Williams").

-compile([debug_info]).

-export([default_map/0, print_map/1]).

-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
-endif.

-define(TYPES, [forest, marsh, mountains, river, clear, hills, rough, station]).
-define(MP,    [2, 3, 4, 4, 1, 3, 3, 1]).

default_map() ->
    [
      [forest, forest, forest, forest, forest, river, forest, rough, rough, rough, mountains, hills, hills,
       forest, forest, hills, hills, forest, forest, forest], % row 1
      [forest, rough, forest, hills, rough, river, marsh, rough, hills, mountains, mountains, hills, hills,
	   hills, hills, hills, forest, hills, forest], % row 2
	  [rough, rough, hills, hills, rough, river, rough, mountains, mountains, hills, mountains, mountains, hills,
	   mountains, mountains, rough, hills, hills, forest, forest], % row 3
	  [rough, rough, hills, mountains, rough, river, forest, mountains, mountains, mountains, mountains, mountains, mountains, mountains,
	   rought, rough, mountains, hills, hills], % row 4
      [forest, marsh, mountains, river, clear, hills, rough],
      [forest, forest, river, forest], % next to last row
      [forest, stations, river] % last row
    ].

print_map(Map) ->
    print_map(Map, 1).

print_map([], _Line) ->
    io:format("Finished drawing.");
print_map(Map, Line) when length(Map) == 1 ->
    [Last] = Map,
    print_line(Last, Line),
    io:format("Drew last line: ~w~n", [Last]),
    print_map([], Line + 1);
print_map([First | Rest], Line) ->
    io:format("Drawing line: ~b:~w~n", [Line, First]),
    print_line(First, Line),
    print_map(Rest, Line+1).

print_line([], _Line) ->
    io:format("~n");
print_line([First | Rest], Line) ->
    io:format("~p:", [First]),
    print_line(Rest, Line).


%% --------------------------------------------------------------------
%%% Eunit test functions
%% --------------------------------------------------------------------
-ifdef(TEST).
print_map() -> print_map(default_map()).

line_test() ->
  print_line([test1, test2, test3], 1),
  io:format("Printed test line~n"),
  ok.
  
print_map_test() ->
  Twolines = [[test1, test2, test3], [test4, test5, test6]],
  print_map(Twolines, 1),
  ok.

print_empty_map_test() ->
  print_map([], 1),
  ok.

print_map_last_line_test() ->
  Oneline = [[test1, test2, test3]],
  print_map(Oneline, 1),
  ok.

simple_test() ->
  print_map(),
  ?assert(true).

-endif.

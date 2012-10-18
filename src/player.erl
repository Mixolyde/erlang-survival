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

-export([new_player/0, new_player/2]).

-include("survival.hrl").

new_player() ->
	#player{}.
new_player(Name, Xloc) when is_list(Name), is_integer(Xloc)->
    #player{pname=Name, loc={Xloc, 1}}.


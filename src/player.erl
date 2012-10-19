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
new_player(Name, Start) when is_list(Name), is_tuple(Start)->
    #player{pname=Name, loc=Start}.


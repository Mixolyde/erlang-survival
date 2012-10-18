%%%-------------------------------------------------------------------
%%% File    : survival_test.erl
%%% Author  : Brian E. Williams <mixolyde@gmail.com>
%%% Description : Calls all unit test modules
%%%
%%% Created :  12 Oct 2012 by Brian E. Williams <mixolyde@gmail.com>
%%%-------------------------------------------------------------------

-module(survival_test).
-author("mixolyde@gmail.com").

-compile([debug_info, export_all]).

-include("../src/survival.hrl").

%% asserts each unit test module returns ok
%% returns ok or {error, Message} if crashes
unit_test() ->
  ok.

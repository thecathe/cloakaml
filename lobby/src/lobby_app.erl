%%%-------------------------------------------------------------------
%% @doc lobby public API
%% @end
%%%-------------------------------------------------------------------

-module(lobby_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lobby_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

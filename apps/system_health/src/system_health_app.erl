%%%-------------------------------------------------------------------
%% @doc system_health public API
%% @end
%%%-------------------------------------------------------------------

-module(system_health_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    system_health_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

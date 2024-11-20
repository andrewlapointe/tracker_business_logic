%%%-------------------------------------------------------------------
%% @doc package_tracker public API
%% @end
%%%-------------------------------------------------------------------

-module(package_tracker_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    package_tracker_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

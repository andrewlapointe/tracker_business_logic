%%%-------------------------------------------------------------------
%% @doc package_monitor public API
%% @end
%%%-------------------------------------------------------------------

-module(package_monitor_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    package_monitor_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

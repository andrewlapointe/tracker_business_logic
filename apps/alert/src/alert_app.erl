%%%-------------------------------------------------------------------
%% @doc alert public API
%% @end
%%%-------------------------------------------------------------------

-module(alert_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    alert_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

%%%-------------------------------------------------------------------
%% @doc notification public API
%% @end
%%%-------------------------------------------------------------------

-module(notification_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    notification_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

%%%-------------------------------------------------------------------
%% @doc registration public API
%% @end
%%%-------------------------------------------------------------------

-module(registration_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    registration_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

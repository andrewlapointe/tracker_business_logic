%%%-------------------------------------------------------------------
%% @doc calculator public API
%% @end
%%%-------------------------------------------------------------------

-module(calculator_app).

-behaviour(application).

-export([start/2, stop/1, add/2, subtract/2]).

add(Num1, Num2) ->
    Num1 + Num2.

subtract(Num1, Num2) ->
    Num1 - Num2.

start(_StartType, _StartArgs) ->
    calculator_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

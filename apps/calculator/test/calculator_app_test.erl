-module(calculator_app_test).
-include_lib("eunit/include/eunit.hrl").


% Test for the add/2 function
add_test() ->
    ?assertEqual(5, calculator_app:add(2, 3)),
    ?assertEqual(0, calculator_app:add(-1, 1)),
    ?assertEqual(10, calculator_app:add(7, 3)).

% Test for the subtract/2 function
subtract_test() ->
    ?assertEqual(1, calculator_app:subtract(3, 2)),
    ?assertEqual(-2, calculator_app:subtract(1, 3)),
    ?assertEqual(0, calculator_app:subtract(5, 5)).

% Test for the start/2 function (basic test to check if it doesn't fail)
start_test() ->
    {ok, _Pid} = calculator_app:start(normal, []),
    ok.

% Test for the stop/1 function
stop_test() ->
    ?assertEqual(ok, calculator_app:stop([])).

simple_test() ->
    ?assertEqual(1, 1).

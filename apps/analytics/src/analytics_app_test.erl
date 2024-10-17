-module(analytics_app_test).
-include_lib("eunit/include/eunit.hrl").

%% Setup and Teardown for each test
setup() ->
    case analytics_service:start_link() of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

teardown(Pid) ->
    gen_statem:stop(Pid).

%% Test 1: Start the state machine and verify it started
start_state_machine_test_() ->
    Pid = setup(),
    ?_assert(is_pid(Pid)),
    teardown(Pid),
    ok.

%% Test 2: Happy path test for tracking a package
track_package_happy_path_test_() ->
    Pid = setup(),
    ?_assertEqual(ok, analytics_service:track_package(1, {10, 0, 0})),
    teardown(Pid).

%% Test 3: Test the "out" state transition (below threshold)
out_state_transition_test_() ->
    Pid = setup(),
    ?_assertEqual(ok, gen_statem:call(Pid, {package_update, 1, out, {10, 30, 0}})),
    teardown(Pid).

%% Test 4: Simulate case where time threshold is exceeded
threshold_exceeded_test_() ->
    Pid = setup(),
    meck:new(log_handler, [unstick]),  % Mock the log handler
    meck:expect(log_handler, log_event, fun(_, _) -> ok end),  % Set up expectation for the mock
    ?_assertEqual(ok, gen_statem:call(Pid, {package_update, 1, out, {11, 0, 0}})),
    meck:unload(log_handler),  % Clean up mock after test
    teardown(Pid).

%% Test 5: Verify that log_event was called when time exceeds threshold
log_event_called_test_() ->
    Pid = setup(),
    meck:new(log_handler, [unstick]),
    meck:expect(log_handler, log_event, fun(_, _) -> ok end),
    ?_assertEqual(ok, gen_statem:call(Pid, {package_update, 1, out, {11, 0, 0}})),
    ?_assert(meck:called(log_handler, log_event, [1, 3600])),  % 1 hour = 3600 seconds
    meck:unload(log_handler),
    teardown(Pid).

%% Run all tests
init_test_() ->
    [
        start_state_machine_test_,
        track_package_happy_path_test_,
        out_state_transition_test_,
        threshold_exceeded_test_,
        log_event_called_test_
    ].

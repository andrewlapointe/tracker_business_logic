-module(analytics_app_test).
-include_lib("eunit/include/eunit.hrl").

<<<<<<< Updated upstream
%% Setup and Teardown for each test
setup() ->
    case analytics_service:start_link() of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

teardown(Pid) ->
    gen_statem:stop(Pid).

%% Test 1: Start the state machine and verify it started
start_state_machine_test() ->
    Pid = setup(),
    ?_assert(is_pid(Pid)),
    teardown(Pid),
    ok.

%% Test 2: Happy path test for tracking a package
track_package_happy_path_test() ->
    Pid = setup(),
    ?_assertEqual(ok, analytics_service:track_package(1, {10, 0, 0})),
    teardown(Pid),
    ok.

%% Test 3: Test the "out" state transition (below threshold)
out_state_transition_test() ->
    Pid = setup(),
    ?_assertEqual(ok, gen_statem:call(Pid, {package_update, 1, out, {10, 30, 0}})),
    teardown(Pid),
    ok.

%% Test 4: Simulate case where time threshold is exceeded
threshold_exceeded_test() ->
    Pid = setup(),
    
    %% Unload any existing mock (defensive)
    catch meck:unload(log_handler),

    %% Ensure the mock is created
    meck:new(log_handler, [unstick]),  % Mock the log handler
    meck:expect(log_handler, log_event, fun(_, _) -> ok end),  % Set up expectation for the mock
    
    %% Perform the state machine call
    ?_assertEqual(ok, gen_statem:call(Pid, {package_update, 1, out, {11, 0, 0}})),

    %% Verify the mock was called as expected
    ?_assert(meck:called(log_handler, log_event, [1, 3600])),

    %% Clean up
    meck:unload(log_handler),  % Unload the mock after test
    teardown(Pid),
    ok.

%% Test 5: Verify that log_event was called when time exceeds threshold
log_event_called_test() ->
    Pid = setup(),
=======
%% Setup and Teardown
setup_and_start_link(PackageId) ->
>>>>>>> Stashed changes
    meck:new(log_handler, [unstick]),
    {ok, Pid} = analytics_service:start_link(PackageId),
    timer:sleep(100),  %% Ensure process is initialized
    Pid.

teardown() ->
    meck:unload(log_handler).

%% Test: Starting the Service
start_link_test_() ->
    %% Happy path with valid PackageId
    Pid = setup_and_start_link(1),
    ?_assertMatch(pid, Pid),
    teardown(),

    %% Edge case: Invalid PackageId
    ?_assertError(badarg, analytics_service:start_link(undefined)).

%% Test: Tracking a Package
track_package_test_() ->
    setup_and_start_link(1),

    %% Happy path
    ?_assertEqual(ok, analytics_service:track_package(1, {{2023, 10, 20}, {12, 0, 0}})),
    
    %% Edge case: Invalid PackageId
    ?_assertEqual({error, invalid_package_id}, analytics_service:track_package(undefined, {{2023, 10, 20}, {12, 0, 0}})),

    teardown().

%% Test: Registered State Transition
registered_state_test_() ->
    setup_and_start_link(1),
    InitialTime = {{2023, 10, 20}, {12, 0, 0}},
    analytics_service:track_package(1, InitialTime),

    %% Transition to 'out' state
    ?_assertEqual(ok, gen_statem:call({analytics_service, 1}, {package_update, 1, out, {{2023, 10, 20}, {12, 5, 0}}})),

    teardown().

%% Test: Out State with Logging
out_state_test_() ->
    setup_and_start_link(1),
    InitialTime = {{2023, 10, 20}, {12, 0, 0}},
    analytics_service:track_package(1, InitialTime),

    %% Mock log_event in log_handler
    meck:expect(log_handler, log_event, fun(_PackageId, _TimeDiff) -> ok end),

    %% Time difference exceeds threshold
    OutTime = {{2023, 10, 20}, {12, 25, 0}}, %% 25 minutes later
    ?_assertEqual(ok, gen_statem:call({analytics_service, 1}, {package_update, 1, out, OutTime})),
    ?_assert(meck:called(log_handler, log_event, [1, 1500])), %% 1500 seconds

    %% Time difference under threshold (no log)
    OutTimeUnderThreshold = {{2023, 10, 20}, {12, 15, 0}}, %% 15 minutes later
    ?_assertEqual(ok, gen_statem:call({analytics_service, 1}, {package_update, 1, out, OutTimeUnderThreshold})),
    LogHistory = meck:history(log_handler),
    ?_assertEqual(1, length([Call || {log_event, _Args} = Call <- LogHistory, Call == {log_event, [1, 1500]}])),

    teardown().

%% Test: Handling Unexpected Events
unexpected_events_test_() ->
    setup_and_start_link(1),
    InitialTime = {{2023, 10, 20}, {12, 0, 0}},
    analytics_service:track_package(1, InitialTime),

    %% Handle unexpected events
    ?_assertEqual({keep_state_and_data, #{}}, gen_statem:call({analytics_service, 1}, unexpected_event)),

    teardown().

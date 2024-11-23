-module(analytics_statem_test).
-include_lib("eunit/include/eunit.hrl").

setup() -> 
    %% Set a custom environment flag for test mode in the process dictionary
    erlang:put(is_test, true),
    
    %% Start the state machine or get its Pid if already started
    case catch analytics_statem:start_link() of
        {ok, Pid} -> {Pid, "/tmp/package_log.txt"};
        {error, {already_started, Pid}} -> {Pid, "/tmp/package_log.txt"}
    end.


%% The test teardown to clear the `is_test` flag and stop the process
teardown({Pid, _LogFilePath}) -> 
    %% Clear the environment variable for the next test
    erlang:erase(is_test),
    
    %% Ensure that the gen_statem process is stopped
    case catch gen_statem:stop(Pid) of
        ok -> ok;
        {error, noproc} -> ok  %% Ignore if the process is already stopped
    end,
    ok.


%% Check if the process is running
start_link_test() -> 
    case catch analytics_statem:start_link() of
        {ok, _Pid} -> 
            %% Ensure process is alive before continuing
            ?assertMatch({ok, _}, analytics_statem:start_link());
        {error, {already_started, Pid}} -> 
            %% Process already started, continue with the test
            ?assertNotEqual(Pid, undefined)
    end,
    ok.


%% Test registering a package
track_package_register_test() -> 
    {Pid2, _LogFilePath} = setup(),
    Time = {10, 30, 0},
    PackageId = "123",
    
    %% Ensure the process is alive
    ?assertMatch({ok, _}, gen_statem:call(Pid2, {track, PackageId, Time}, infinity)),
    
    %% Get the state and assert that the package is registered with the correct time
    StateData = analytics_statem:state(),
    PackageState = maps:get(PackageId, StateData, #{error => "not_found"}),
    ?assertEqual(#{state => registered, registered_time => Time}, PackageState),
    
    %% Call the teardown manually (cleanup after the test)
    teardown({Pid2, _LogFilePath}).


%% Test the transition to 'out' state and the time difference calculation
track_package_out_test() -> 
    {Pid3, _LogFilePath} = setup(),
    PackageId = "123",
    RegisteredTime = {10, 30, 0},
    OutTime = {10, 35, 0},
    
    %% Register the package
    analytics_statem:track_package(PackageId, RegisteredTime),
    
    %% Simulate the transition to 'out' state
    {next_state, out, NewStateData, _} = analytics_statem:out({call, self()},
        {package_update, PackageId, out, OutTime, "/tmp/package_log.txt"},
        analytics_statem:state()),
    
    %% Assert the time difference calculation (5 minutes = 300 seconds)
    TimeDiff = 300,
    PackageState = maps:get(PackageId, NewStateData, #{error => "not_found"}),
    ?assertEqual(#{state => out, time_diff => TimeDiff}, PackageState),
    
    %% Call the teardown manually (cleanup after the test)
    teardown({Pid3, _LogFilePath}).

%% Test calculating time difference
time_difference_calculation_test() -> 
    Time1 = {10, 30, 0},    %% 10:30:00
    Time2 = {10, 35, 0},    %% 10:35:00
    ExpectedDiff = 300,     %% 5 minutes = 300 seconds
    
    %% Call the time difference calculation
    Result = analytics_statem:calculate_time_difference(Time1, Time2),
    
    %% Assert the result matches the expected difference
    ?assertEqual(ExpectedDiff, Result).

%% Test default handler for unrecognized events
handle_event_default_test() -> 
    {Pid4, _LogFilePath} = setup(),
    State = registered,
    Data = #{} ,
    EventType = call,
    EventContent = {unknown_event},
    %% This should return the current state and data without modifying anything
    {keep_state_and_data, Data} = analytics_statem:handle_event(EventType, EventContent, State, Data),
    
    %% Call the teardown manually (cleanup after the test)
    teardown({Pid4, _LogFilePath}).

%% Test termination behavior
terminate_test() -> 
    %% Simulate the termination and ensure no errors occur
    ?assertMatch(ok, analytics_statem:terminate(normal, registered, #{})),
    
    %% Ensure the gen_statem process is stopped after the test
    %% We no longer need to stop the process manually as the `teardown/1` handles that cleanup.
    ok.

-module(tracking_app_test).
-include_lib("eunit/include/eunit.hrl").

%% Export the test function
-export([tracking_service_test/0]).

%% Setup the mock for data_interaction and start the gen_server
setup() ->
    %% Start a mock of the data_interaction module
    meck:new(data_interaction, [unstick]),
    
    %% Define behavior for the mocked function
    meck:expect(data_interaction, get_package_status, fun
        ("valid_package") -> {ok, #{status => "delivered"}};
        ("missing_package") -> {error, not_found};
        (_) -> {error, "unknown error"}
    end),
    
    %% Start the gen_server
    {ok, _Pid} = tracking_server:start_link(),
    ok.

%% Cleanup the mock and stop the gen_server
cleanup() ->
    meck:unload(data_interaction),
    ok.

%% The main test function, calling the setup and cleanup hooks
tracking_service_test() ->
    setup(),
    %% Test: Valid package status
    ?assertEqual({ok, #{status => "delivered"}},
                 tracking_server:get_status("valid_package")),

    %% Test: Missing package
    ?assertEqual({error, "Package not found"},
                 tracking_server:get_status("missing_package")),

    %% Test: Unknown error case
    ?assertEqual({error, "unknown error"},
                 tracking_server:get_status("some_package")),

    %% Call cleanup after tests
    cleanup().

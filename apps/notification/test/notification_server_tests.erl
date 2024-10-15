-module(notification_service_tests).
-include_lib("eunit/include/eunit.hrl").
-import(notification_service, [start_link/0, notify/2, get_status/1]).

%% Test for start_link/0
start_link_test() ->
    {ok, Pid} = notification_service:start_link(),
    ?assert(is_pid(Pid)).

%% Test for get_status/1 when the package is found
get_status_found_test() ->
    %% Assuming riak_kv:put/2 is mocked here to return a successful response
    PackageId = <<"12345">>,
    riak_kv:put(PackageId, #{status => delivered}),
    {ok, Status} = notification_service:get_status(PackageId),
    ?assertEqual({ok, #{status => delivered}}, Status).

%% Test for get_status/1 when the package is not found
get_status_not_found_test() ->
    PackageId = <<"54321">>,
    {error, Reason} = notification_service:get_status(PackageId),
    ?assertEqual(Reason, "Package not found").

%% Test for get_status/1 when Riak returns an error
get_status_error_test() ->
    PackageId = <<"99999">>,
    riak_kv:delete(PackageId),  % Simulate error condition
    {error, Reason} = notification_service:get_status(PackageId),
    ?assertEqual(Reason, not_found).

%% Run all tests
all_tests() ->
    [start_link_test, get_status_found_test, get_status_not_found_test, get_status_error_test].


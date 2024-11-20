-module(package_monitor_app_test).
-include_lib("eunit/include/eunit.hrl").

%% Tests
%% Test for happy path
%% Test for happy path
%% Test for happy path
update_db_record_test() ->
    %% Start notification service to ensure event manager is running
    notification_app:start_link(),

    %% Mock riak_kv, notification_app, and io:format to prevent I/O errors
    meck:new(pm_db, [unstick]),
    meck:new(notification_app, [unstick]),
    meck:new(io, [unstick]),
    meck:expect(io, format, fun(_Fmt, _Args) -> ok end),

    %% Mock riak_kv:put and notification_app:notify
    meck:expect(pm_db, put, fun(_PackageId, _Data) -> ok end),
    meck:expect(notification_app, notify, fun(_PackageId, _Data) -> ok end),

    %% Start the package monitor service and trigger the update
    package_monitor_app:start_link(),
    package_monitor_app:update_db_record("package_123", #{key => value}),

    %% Check if riak_kv:put and notification_app:notify were called
    ?_assert(meck:called(pm_db:put("package_123", #{key => value}))),
    ?_assert(meck:called(notification_app:notify("package_123", #{key => value}))),

    %% Unload mocks
    meck:unload(pm_db),
    meck:unload(notification_app),
    meck:unload(io),

    ok.

update_db_record_failure_test() ->
    %% Nasty path: riak_kv:put/2 returns an error, notification_app:notify/2 is NOT called
    meck:new(pm_db, [unstick]),
    meck:new(notification_app, [unstick]),

    meck:expect(pm_db, put, fun(_PackageId, _Data) -> {error, reason} end),
    meck:expect(notification_app, notify, fun(_PackageId, _Data) -> ok end),

    package_monitor_app:start_link(),
    package_monitor_app:update_db_record("package_456", #{key => "invalid"}),

    ?_assert(meck:called(pm_db:put("package_456", #{key => "invalid"}))),
    ?_assertNot(meck:called(notification_app:notify("package_456", #{key => "invalid"}))),

    meck:unload(pm_db),
    meck:unload(notification_app),

    %% Ensure to return ok at the end of the test.
    ok.

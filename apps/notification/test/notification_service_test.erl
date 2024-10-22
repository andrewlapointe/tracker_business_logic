-module(notification_service_test).
-include_lib("eunit/include/eunit.hrl").

start_link_test_() ->
    % happy path for start_link/0
    [
        ?_assertMatch({ok, _Pid}, notification_service:start_link()),
        % happy path for start_link/1
        ?_assertMatch({ok, _Pid}, notification_service:start_link(custom_name))
    ].

notify_test_() ->
    % happy path for notify/2
    [
        ?_assertEqual(ok, notification_service:notify(package1, shipped)),
        % happy path for notify/3
        ?_assertEqual(ok, notification_service:notify(custom_name, package1, delivered)),
        % edge case: undefined event manager, expect a badarg error
        ?_assertException(
            error, badarg, notification_service:notify(undefined_name, package1, in_transit)
        ),
        % edge case: undefined package, no exception expected now
        ?_assertEqual(
            ok, notification_service:notify(notification_service_event, undefined_package, lost)
        )
    ].

send_notification_via_https_test_() ->
    % happy path
    [
        ?_assertEqual(ok, notification_service:send_notification_via_https(package1, delivered)),
        % happy path for different status
        ?_assertEqual(ok, notification_service:send_notification_via_https(package1, lost)),
        % edge case: undefined package
        ?_assertEqual(
            ok, notification_service:send_notification_via_https(undefined_package, unknown)
        ),
        % edge case: undefined status
        ?_assertEqual(
            ok, notification_service:send_notification_via_https(package2, undefined_status)
        )
    ].

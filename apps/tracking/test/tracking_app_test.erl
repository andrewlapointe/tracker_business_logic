-module(tracking_app_test).
-include_lib("eunit/include/eunit.hrl").

% Setup function to initialize the meck mock
setup() ->
    meck:new(data_interaction, [unstick]).

% Teardown function to unload the meck mock
teardown() ->
    meck:unload(data_interaction).

% Test case for get_status/1 function
get_status_test_() ->
    setup(),

    % Define how get_package_status/1 should behave when called
    meck:expect(data_interaction, get_package_status, fun(PackageId) -> 
        case PackageId of
            123 ->
                {ok, #{status => "In Transit"}};  % Happy path
            456 ->
                {error, not_found};  % Package not found
            789 ->
                {error, timeout};  % Simulate timeout error
            _ ->
                {error, unknown_error}  % Default case for undefined PackageId
        end
    end),

    % Return a list of assertions
    Assertions = [
        % Happy path: Package found
        ?_assertEqual({ok, #{status => "In Transit"}}, tracking_service:get_status(123)),

        % Edge case: Package not found
        ?_assertEqual({error, "Package not found"}, tracking_service:get_status(456)),

        % Edge case: Other error (e.g., timeout)
        ?_assertEqual({error, timeout}, tracking_service:get_status(789))
    ],

    teardown(),

    % Return the list of assertions
    Assertions.

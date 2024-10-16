-module(data_interaction).
-export([get_package_status/1]).

% Dummy implementation of the get/1 function
get_package_status(PackageId) ->
    case PackageId of
        123 ->
            {ok, #{status => "In Transit"}};  % Happy path
        456 ->
            {error, not_found};  % Package not found
        789 ->
            {error, timeout};  % Simulate timeout error
        _ ->
            {error, unknown_error}  % Default case for undefined PackageId
    end.

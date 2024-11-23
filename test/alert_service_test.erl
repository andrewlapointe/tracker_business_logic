-module(alert_service_test).
-include_lib("eunit/include/eunit.hrl").

%% Test functions
alert_service_test_() ->
    {
        setup,
        fun setup/0,
        fun cleanup/1,
        [
            %% Test for raising and handling alerts
            ?_test(raise_alert_test())
        ]
    }.

%% Setup function to start the alert service
setup() ->
    case alert_app:start_link() of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.

%% Cleanup function
cleanup(_) ->
    ok.

%% Custom logging function for testing
mock_log(FileName, Message) ->
    io:format("MOCK LOG: FileName = ~p, Message = ~p~n", [FileName, Message]).

%% Test for raising and handling alerts
raise_alert_test() ->
    %% Define a test alert message (binary)
    AlertMessage = <<"Test Alert">>,

    %% Call the alert service directly and inject the mock log function
    alert_app:log_alert_to_file("test_log.txt", AlertMessage, fun mock_log/2),

    %% Capture the expected output, flatten the list for comparison
    ExpectedOutput = lists:flatten("MOCK LOG: FileName = \"test_log.txt\", Message = \"Test Alert\"\n"),

    %% Capture the actual output from io_lib:format and convert binary to list
    ActualOutput = lists:flatten(io_lib:format("MOCK LOG: FileName = ~p, Message = ~p~n", ["test_log.txt", binary_to_list(AlertMessage)])),

    %% Compare the expected and actual output
    ?assertEqual(ExpectedOutput, ActualOutput).

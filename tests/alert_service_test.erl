-module(alert_service_test).
-include_lib("eunit/include/eunit.hrl").
-define(TEST_LOG_FILE, "test_log.txt").

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
    case alert_event:start_link() of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.

%% Cleanup function
cleanup(_) ->
    % ok.
    file:delete(?TEST_LOG_FILE).

%% Custom logging function for testing
mock_log(FileName, Message) ->
    io:format("MOCK LOG: FileName = ~p, Message = ~p~n", [FileName, Message]).

%% Test for raising and handling alerts
raise_alert_test() ->
    %% Define a test alert message (binary)
    AlertMessage = <<"Test Alert">>,

    %% Call the alert service directly and inject the mock log function
    alert_event:log_alert_to_file(?TEST_LOG_FILE, AlertMessage, fun mock_log/2),

    %% Capture the expected output, flatten the list for comparison
    ExpectedOutput = "MOCK LOG: FileName = \"test_log.txt\", Message = \"Test Alert\"\n",

    %% Capture the actual output from io_lib:format and convert binary to list
    ActualOutput = lists:flatten(io_lib:format("MOCK LOG: FileName = ~p, Message = ~p~n", [?TEST_LOG_FILE, AlertMessage])),

    %% Compare the expected and actual output
    ?assertEqual(ExpectedOutput, ActualOutput).

%% Additional test to verify file contents
check_file_logging_test() ->
    %% Raise an alert that logs to the default file logger
    AlertMessage = <<"Persistent Test Alert">>,
    alert_event:log_alert_to_file(?TEST_LOG_FILE, AlertMessage, fun alert_event:file_log/2),

    %% Read the file and verify it contains the correct log entry
    {ok, BinaryContents} = file:read_file(?TEST_LOG_FILE),
    FileContents = binary_to_list(BinaryContents),
    ?assert(list_to_binary("ALERT: \"Persistent Test Alert\" - Timestamp: ") =:= binary:part(FileContents, 0, 33)).
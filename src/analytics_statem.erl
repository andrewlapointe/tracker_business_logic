-module(analytics_statem).
-behaviour(gen_statem).

% ==================================
% TODO: This does not work. Needs to record data somewhere. Could be a text file or similar.
% ==================================

%% API
-export([start_link/0, track_package/2]).
-export([init/1, callback_mode/0, registered/3, out/3, handle_event/4, terminate/3, code_change/4, state/0, log_to_file_if_needed/3, calculate_time_difference/2]).

%% Client API
start_link() -> 
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

track_package(PackageId, InitialTime) -> 
    gen_statem:call(?MODULE, {track, PackageId, InitialTime}).

init([]) -> 
    {ok, registered, #{} }.

callback_mode() -> 
    state_functions.

%% State: registered
%% Records the time when the package is registered.
registered({call, _From}, {track, PackageId, Time}, StateData) -> 
    maybe_io_format("Package ~p registered at time ~p~n", [PackageId, Time]),
    NewStateData = maps:put(PackageId, #{state => registered, registered_time => Time}, StateData),
    {next_state, out, NewStateData, [{reply, _From, ok}]}.

%% State: out
%% Records the time when the package is out for delivery and calculates time differences.
out({call, _From}, {package_update, PackageId, out, Time, LogFilePath}, StateData) -> 
    maybe_io_format("Package ~p updated to 'out' state at time ~p~n", [PackageId, Time]),
    case maps:get(PackageId, StateData, undefined) of 
        undefined ->
            io:format("No data found for package ~p~n", [PackageId]),
            {keep_state, StateData, [{reply, {error, "No registered time found"}}]};
        #{registered_time := RegisteredTime} = Data ->
            TimeDiff = calculate_time_difference(RegisteredTime, Time),
            maybe_io_format("Time difference for package ~p is ~p seconds~n", [PackageId, TimeDiff]),
            log_to_file_if_needed(PackageId, TimeDiff, LogFilePath),
            UpdatedData = maps:put(PackageId, #{state => out, time_diff => TimeDiff}, Data),
            NewStateData = maps:put(PackageId, UpdatedData, StateData),
            {next_state, out, NewStateData, [{reply, _From, ok}]}
    end.

%% Default handler for other events
handle_event(_EventType, _EventContent, State, Data) -> 
    maybe_io_format("Event received in state ~p: ~p~n", [State, _EventContent]),
    {keep_state_and_data, Data}.

terminate(_Reason, _State, _Data) -> 
    maybe_io_format("State machine terminating~n", []),
    ok.

code_change(_OldVsn, State, Data, _Extra) -> 
    {ok, State, Data}.

%% Calculate the time difference between the registered and out times
calculate_time_difference({Hr1, Min1, Sec1}, {Hr2, Min2, Sec2}) -> 
    Time1 = Hr1 * 3600 + Min1 * 60 + Sec1, 
    Time2 = Hr2 * 3600 + Min2 * 60 + Sec2, 
    abs(Time2 - Time1).

log_to_file_if_needed(PackageId, TimeDiff, LogFilePath) -> 
    %% If we're in test mode, avoid writing logs to file
    case erlang:get(is_test) of
        true -> 
            ok;  %% In test mode, don't write to file
        false -> 
            Threshold = 20 * 60, %% 20 minutes in seconds
            case TimeDiff > Threshold of
                true -> 
                    {Date, {Hour, Minute, Second}} = calendar:local_time(),
                    {Year, Month, Day} = Date,
                    Timestamp = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                              [Year, Month, Day, Hour, Minute, Second]),
                    LogMessage = io_lib:format("Timestamp: ~s | Package: ~p | Time Difference: ~p seconds~n",
                                               [Timestamp, PackageId, TimeDiff]),
                    append_to_log_file(LogMessage, LogFilePath);
                false -> 
                    maybe_io_format("No log required for package ~p: TimeDiff = ~p seconds~n", [PackageId, TimeDiff]),
                    ok
            end
    end.


append_to_log_file(Message, FilePath) -> 
    maybe_io_format("Attempting to write log: ~s to file: ~s~n", [Message, FilePath]),
    case file:open(FilePath, [append]) of
        {ok, IoDevice} -> 
            ok = file:write(IoDevice, Message), 
            file:close(IoDevice), 
            maybe_io_format("Log successfully written to ~s~n", [FilePath]);
        {error, Reason} -> 
            maybe_io_format("Failed to write log to ~s: ~p~n", [FilePath, Reason]),
            error
    end.

state() -> 
    %% Return the current state and data, instead of just a placeholder
    case gen_statem:call(?MODULE, {get_state}) of
        {ok, StateData} -> {registered, StateData};
        _ -> {registered, #{}}
    end.

%% Helper function to handle `io:format` in test and non-test environments
maybe_io_format(FormatString, Args) ->
    %% Default to false if is_test is not found in the process dictionary
    case erlang:get(is_test) of
        undefined -> 
            %% If 'is_test' is not set, default to false
            ok;
        true -> 
            ok; %% If we're in a test environment, do nothing
        false -> 
            io:format(FormatString, Args) %% Otherwise, perform the format
    end.

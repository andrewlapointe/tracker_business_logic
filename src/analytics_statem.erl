-module(analytics_statem).
-behaviour(gen_statem).

-export([start_link/0, track_package/2, update_package/2]).
-export([init/1, callback_mode/0, registered/3, out/3, handle_event/4, terminate/3, code_change/4]).

%% API
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

track_package(PackageId, InitialTime) ->
    gen_statem:call(?MODULE, {track, PackageId, InitialTime}).

update_package(PackageId, Time) ->
    gen_statem:call(?MODULE, {package_update, PackageId, out, Time}).

%% State Initialization
init([]) ->
    FileName = "package_logs.txt",
    case file:open(FileName, [append]) of
        {ok, IoDevice} ->
            {ok, registered, #{log_file => IoDevice, packages => #{}}};
        {error, Reason} ->
            io:format("Failed to open log file: ~p~n", [Reason]),
            {stop, Reason}
    end.

callback_mode() -> state_functions.

%% State: registered
registered({call, From}, {track, PackageId, Time}, StateData) ->
    io:format("Package ~p registered at time ~p~n", [PackageId, Time]),
    %% Update state with registration time
    UpdatedPackages = maps:put(PackageId, #{state => registered, registered_time => Time}, maps:get(packages, StateData)),
    NewStateData = StateData#{packages => UpdatedPackages},
    {next_state, out, NewStateData, [{reply, From, ok}]}.

%% State: out
out({call, From}, {package_update, PackageId, out, Time}, StateData) ->
    io:format("Package ~p updated to 'out' state at time ~p~n", [PackageId, Time]),
    case maps:get(PackageId, maps:get(packages, StateData), undefined) of
        #{registered_time := RegisteredTime} = PackageData ->
            TimeDiff = calculate_time_difference(RegisteredTime, Time),
            io:format("Time difference for package ~p is ~p seconds~n", [PackageId, TimeDiff]),
            %% Log the event if needed
            LogFile = maps:get(log_file, StateData),
            log_to_file(LogFile, PackageId, TimeDiff),

            %% Update package data in state
            UpdatedPackageData = maps:put(PackageId, #{state => out, time_diff => TimeDiff}, PackageData),
            UpdatedPackages = maps:put(PackageId, UpdatedPackageData, maps:get(packages, StateData)),
            NewStateData = StateData#{packages => UpdatedPackages},
            {keep_state, NewStateData, [{reply, From, ok}]};
        undefined ->
            {keep_state, StateData, [{reply, From, {error, "No registered time found"}}]}
    end.

%% Default event handler
handle_event(_EventType, _EventContent, State, Data) ->
    io:format("Event received in state ~p: ~p~n", [State, _EventContent]),
    {keep_state_and_data, Data}.

%% State Machine Termination
terminate(_Reason, _State, StateData) ->
    %% Close the log file
    case maps:get(log_file, StateData, undefined) of
        undefined -> ok;
        IoDevice -> file:close(IoDevice)
    end,
    io:format("State machine terminating~n"),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%% Calculate time difference between registered and out times
calculate_time_difference({Hr1, Min1, Sec1}, {Hr2, Min2, Sec2}) ->
    Time1 = Hr1 * 3600 + Min1 * 60 + Sec1,
    Time2 = Hr2 * 3600 + Min2 * 60 + Sec2,
    abs(Time2 - Time1).

%% Log to file
log_to_file(IoDevice, PackageId, TimeDiff) ->
    Threshold = 20 * 60, %% 20 minutes in seconds
    case TimeDiff > Threshold of
        true ->
            LogMessage = io_lib:format("Package ~p: TimeDiff = ~p seconds~n", [PackageId, TimeDiff]),
            case file:write(IoDevice, LogMessage) of
                ok ->
                    file:sync(IoDevice), %% Ensure content is flushed
                    ok;
                {error, Reason} ->
                    io:format("Failed to write to log file: ~p~n", [Reason]),
                    {error, Reason}
            end;
        false ->
            io:format("No log required for package ~p: TimeDiff = ~p seconds~n", [PackageId, TimeDiff]),
            ok
    end.


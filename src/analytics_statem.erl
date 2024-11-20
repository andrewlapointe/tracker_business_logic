-module(analytics_statem).
-behaviour(gen_statem).

% ==================================
% TODO: This does not work. Needs to record data somewhere. Could be a text file or similar.
% ==================================

%% API
-export([start_link/0, track_package/2]).
-export([init/1, callback_mode/0, registered/3, out/3, handle_event/4, terminate/3, code_change/4]).

%% Client API
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

track_package(PackageId, InitialTime) ->
    gen_statem:call(?MODULE, {track, PackageId, InitialTime}).

init([]) ->
    {ok, registered, #{}}.

callback_mode() ->
    state_functions.

%% State: registered
%% Records the time when the package is registered.
registered({call, _From}, {track, PackageId, Time}, StateData) ->
    io:format("Package ~p registered at time ~p~n", [PackageId, Time]),
    %% Update the state with the registration time
    NewStateData = maps:put(PackageId, #{state => registered, registered_time => Time}, StateData),
    {next_state, out, NewStateData, [{reply, _From, ok}]}.

%% State: out
%% Records the time when the package is out for delivery and calculates time differences.
out({call, _From}, {package_update, PackageId, out, Time}, StateData) ->
    io:format("Package ~p updated to 'out' state at time ~p~n", [PackageId, Time]),
    case maps:get(PackageId, StateData) of
        #{registered_time := RegisteredTime} = Data ->
            TimeDiff = calculate_time_difference(RegisteredTime, Time),
            io:format("Time difference for package ~p is ~p seconds~n", [PackageId, TimeDiff]),
            %% Log the event through a handler if it exceeds the threshold
            log_to_file_if_needed(PackageId, TimeDiff),

            UpdatedData = maps:put(PackageId, #{state => out, time_diff => TimeDiff}, Data),
            NewStateData = maps:put(PackageId, UpdatedData, StateData),
            {next_state, out, NewStateData, [{reply, _From, ok}]};
        _ ->
            {keep_state, StateData, [{reply, {error, "No registered time found"}}]}
    end.

%% Default handler for other events
handle_event(_EventType, _EventContent, State, Data) ->
    io:format("Event received in state ~p: ~p~n", [State, _EventContent]),
    {keep_state_and_data, Data}.

terminate(_Reason, _State, _Data) ->
    case catch io:format("State machine terminating~n") of
        {'EXIT', _} -> ok;  % Ignore IO errors (e.g., device termination)
        _ -> ok
    end.


code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%% Calculate the time difference between the registered and out times
calculate_time_difference({Hr1, Min1, Sec1}, {Hr2, Min2, Sec2}) ->
    %% Convert both times to seconds
    Time1 = Hr1 * 3600 + Min1 * 60 + Sec1,
    Time2 = Hr2 * 3600 + Min2 * 60 + Sec2,
    %% Return the absolute difference in seconds
    abs(Time2 - Time1).

%% Log to file if the time difference is greater than the threshold
log_to_file_if_needed(PackageId, TimeDiff) ->
    Threshold = 20 * 60, %% 20 minutes in seconds
    case TimeDiff > Threshold of
        true ->
            io:format("Logging event for package ~p: TimeDiff = ~p seconds~n", [PackageId, TimeDiff]),
            log_handler:log_event(PackageId, TimeDiff);
        false ->
            io:format("No log required for package ~p: TimeDiff = ~p seconds~n", [PackageId, TimeDiff]),
            ok
    end.

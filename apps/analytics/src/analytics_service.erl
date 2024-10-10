% Analytics Service Using gen_statem

-module(analytics_service).
-behaviour(gen_statem).

%% API
-export([start_link/0, track_package/2]).
-export([init/1, callback_mode/0, registered/3, out/3, handle_event/4, terminate/3, code_change/4]).

%% Client API
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Function to start tracking a package with its initial state
track_package(PackageId, initial_time) ->
    gen_statem:call(?MODULE, {track, PackageId, initial_time}).

%% Initialize the state machine with a default state
init([]) ->
    {ok, registered, #{}}.

callback_mode() ->
    state_functions.

%% State: registered
%% Records the time when the package is registered.
registered({call, _From}, {track, PackageId, Time}, StateData) ->
    %% Update the state with the registration time
    NewStateData = maps:put(PackageId, #{state => registered, registered_time => Time}, StateData),
    {next_state, out, NewStateData, [{reply, ok}]}.

%% State: out
%% Records the time when the package is out for delivery and calculates time differences.
out({call, _From}, {package_update, PackageId, out, Time}, StateData) ->
    %% Get the registered time
    case maps:get(PackageId, StateData) of
        #{registered_time := RegisteredTime} = Data ->
            %% Calculate the time difference
            TimeDiff = calculate_time_difference(RegisteredTime, Time),
            %% Log the event through a handler if it exceeds the threshold
            log_to_file_if_needed(PackageId, TimeDiff),
            %% Update the state data
            UpdatedData = maps:put(PackageId, #{state => out, time_diff => TimeDiff}, Data),
            NewStateData = maps:put(PackageId, UpdatedData, StateData),
            {next_state, out, NewStateData, [{reply, ok}]};
        _ ->
            {keep_state, StateData, [{reply, {error, "No registered time found"}}]}
    end.

%% Default handler for other events
handle_event(_EventType, _EventContent, State, Data) ->
    {keep_state_and_data, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%% Calculate the time difference between the registered and out times
calculate_time_difference(RegisteredTime, OutTime) ->
    calendar:time_difference(RegisteredTime, OutTime).

%% Log to file if the time difference is greater than the threshold
log_to_file_if_needed(PackageId, TimeDiff) ->
    Threshold = 20 * 60, %% 20 minutes in seconds
    case TimeDiff > Threshold of
        true ->
            log_handler:log_event(PackageId, TimeDiff);
        false ->
            ok
    end.

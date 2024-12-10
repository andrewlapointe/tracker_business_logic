-module(analytics_statem).
-behaviour(gen_statem).

-export([start_link/0, track_package/2, update_package/3]).
-export([init/1, callback_mode/0, registered/3, out/3, delivered/3, handle_event/4, terminate/3, code_change/4]).

%% API
start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

track_package(PackageId, InitialTime) ->
    gen_statem:call(?MODULE, {track, PackageId, InitialTime}).

update_package(PackageId, NewState, Time) ->
    gen_statem:call(?MODULE, {package_update, PackageId, delivered, Time}).

%% State Initialization
init([]) ->
    Dir = "./logs",
    case filelib:is_dir(Dir) of
        true -> open_log_file(Dir);
        false ->
            case file:make_dir(Dir) of
                ok -> open_log_file(Dir);
                {error, Reason} ->
                    io:format("Failed to create directory: ~p~n", [Reason]),
                    {stop, Reason}
            end
    end.

open_log_file(Dir) ->
    FileName = filename:join([Dir, "package_logs.log"]),
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
    UpdatedPackages = maps:put(PackageId, #{state => registered, registered_time => Time}, maps:get(packages, StateData)),
    NewStateData = StateData#{packages => UpdatedPackages},
    {keep_state, NewStateData, [{reply, From, ok}]};

registered({call, From}, {package_update, PackageId, out, Time}, StateData) ->
    io:format("Package ~p updated to 'out' state at time ~p~n", [PackageId, Time]),
    UpdatedPackages = maps:put(PackageId, #{state => out, registered_time => Time}, maps:get(packages, StateData)),
    NewStateData = StateData#{packages => UpdatedPackages},
    {next_state, out, NewStateData, [{reply, From, ok}]};

registered(EventType, EventContent, StateData) ->
    io:format("Unexpected event in 'registered' state: ~p, ~p~n", [EventType, EventContent]),
    {keep_state_and_data, StateData}.

%% State: out
out({call, From}, {package_update, PackageId, delivered, Time}, StateData) ->
    io:format("Package ~p delivered at time ~p~n", [PackageId, Time]),
    UpdatedPackages = maps:put(PackageId, #{state => delivered, delivered_time => Time}, maps:get(packages, StateData)),
    NewStateData = StateData#{packages => UpdatedPackages},
    {next_state, delivered, NewStateData, [{reply, From, ok}]};

out({call, From}, {track, PackageId, _Time}, StateData) ->
    io:format("Cannot track package ~p in 'out' state~n", [PackageId]),
    {keep_state_and_data, StateData, [{reply, From, {error, "Cannot track in 'out' state"}}]};

out(EventType, EventContent, StateData) ->
    io:format("Unexpected event in 'out' state: ~p, ~p~n", [EventType, EventContent]),
    {keep_state_and_data, StateData}.

%% State: delivered
delivered({call, From}, {package_update, PackageId, _NewState, _Time}, StateData) ->
    io:format("Package ~p is already delivered and cannot be updated~n", [PackageId]),
    {keep_state_and_data, StateData, [{reply, From, {error, "Package already delivered"}}]};

delivered(EventType, EventContent, StateData) ->
    io:format("Unexpected event in 'delivered' state: ~p, ~p~n", [EventType, EventContent]),
    {keep_state_and_data, StateData}.

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

code_change(_OldVsn, State, Data, _Extra) -> {ok, State, Data}.

%% Log to file
log_to_file(IoDevice, PackageId, Message) ->
    LogMessage = io_lib:format("Package ~p: ~s~n", [PackageId, Message]),
    case file:write(IoDevice, LogMessage) of
        ok -> file:sync(IoDevice), ok;
        {error, Reason} -> io:format("Failed to write to log file: ~p~n", [Reason]), {error, Reason}
    end.

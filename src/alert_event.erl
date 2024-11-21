-module(alert_event).
-behaviour(gen_event).

%% API
-export([start_link/0, raise_alert/1]).
-export([init/1, handle_event/2, terminate/2]).

%% Client API
start_link() ->
    gen_event:start_link({local, ?MODULE}).

raise_alert(AlertMessage) ->
    gen_event:notify(?MODULE, {system_alert, AlertMessage}).

%% gen_event callbacks
init([]) ->
    %% Specify the log file path here or pass it dynamically as part of State.
    {ok, #{log_file => "new_alerts.log"}}.

%% Handle the alert event and log to file
handle_event({system_alert, Message}, State) ->
    %% Log to console
    io:format("ALERT RECEIVED: ~p~n", [Message]),

    %% Extract log file path from State
    LogFile = maps:get(log_file, State),
    io:format("Using log file: ~s~n", [LogFile]),

    %% Attempt to log to file
    case log_alert_to_file(LogFile, Message) of
        ok ->
            io:format("Alert successfully logged to file: ~s~n", [LogFile]);
        {error, Reason} ->
            io:format("Failed to log alert to file: ~p~n", [Reason])
    end,
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

ensure_directory_exists(FilePath) ->
    case file:make_dir_all(filename:dirname(FilePath)) of
        ok -> ok;
        {error, Reason} ->
            io:format("Failed to create directory for file: ~p~n", [Reason])
    end.
    
%% Log alerts to a text file
log_alert_to_file(FileName, Message) ->
    ensure_directory_exists(FileName),

    io:format("Opening file: ~s~n", [FileName]),
    case file:open(FileName, [append]) of
        {ok, File} ->
            % Write the data with the current timestamp
            io:format(File, "ALERT: ~s - Timestamp: ~p~n", [Message, calendar:local_time()]),
            file:close(File),
            ok;
        {error, Reason} ->
            io:format("Failed to open file: ~p~n", [Reason]),
            {error, Reason}
    end.




terminate(_Reason, _State) ->
    io:format("Terminating.~n"),
    ok.

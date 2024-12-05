-module(alert_event).
-behaviour(gen_event).

%% API
-export([start_link/0, raise_alert/1]).
-export([init/1, handle_event/2, terminate/2, log_alert_to_file/2, ensure_logs_directory/0]).


%% Client API
start_link() ->
    {ok, Pid} = gen_event:start_link({local, ?MODULE}),
    gen_event:add_handler(?MODULE, ?MODULE, []),
    {ok, Pid}.

raise_alert(AlertMessage) ->
    gen_event:notify(?MODULE, {system_alert, AlertMessage}).

%% gen_event callbacks
init([]) ->
    %% Ensure the logs directory exists
    ensure_logs_directory(),

    %% Specify the log file path inside the logs folder
    LogFile = "./logs/alerts.log",

    %% Debugging: Print the initialized log file path
    io:format("Initializing with log file: ~p~n", [LogFile]),

    %% Initialize the state
    {ok, #{log_file => LogFile}}.

%% Handle the alert event and log to file
handle_event({system_alert, Message}, State) ->
    %% Log to console
    io:format("ALERT RECEIVED: ~p~n", [Message]),

    %% Extract log file path from State
    LogFile = maps:get(log_file, State),

    %% Attempt to log to file
    case log_alert_to_file(LogFile, Message) of
        ok ->
            io:format("Alert successfully logged to file: ~s~n", [LogFile]);
        {error, Reason} ->
            io:format(standard_io, "Failed to log alert to file: ~p~n", [Reason])
    end,
    {ok, State};
handle_event(_Event, State) ->
    io:format("Unhandled event.~n"),
    {ok, State}.

%% Log alerts to a text file
log_alert_to_file(FileName, Message) ->
    case file:open(FileName, [append]) of
        {ok, File} ->
            io:format(File, "ALERT: ~s - Timestamp: ~p~n", [Message, calendar:local_time()]),
            file:close(File),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Ensure the logs directory exists
ensure_logs_directory() ->
    LogDir = "./logs",
    case file:make_dir(LogDir) of
        % Directory already exists
        {error, eexist} ->
            ok;
        % Directory created successfully
        ok ->
            ok;
        {error, Reason} ->
            io:format("Failed to create logs directory. Reason: ~p~n", [Reason]),
            {error, Reason}
    end.

terminate(_Reason, _State) ->
    io:format("Terminating.~n"),
    ok.

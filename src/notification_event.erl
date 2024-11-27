-module(notification_event).
-behaviour(gen_event).

%% API
-export([start_link/1, notify/2, send_notification_to_log/2]).
-export([init/1, handle_event/2, terminate/2]).

%% Client API
start_link(?MODULE) ->
    gen_event:start_link({local, ?MODULE}).

notify(PackageId, Status) ->
    gen_event:notify(notification_manager, {package_update, PackageId, Status}).

%% gen_event callbacks
init([]) ->
    {ok, #{}}.

%% Handle the event for package updates
handle_event({package_update, PackageId, Status}, State) ->
    io:format("Handling event: PackageId=~p, Status=~p~n", [PackageId, Status]),
    send_notification_to_log(PackageId, Status),
    {ok, State}.

ensure_logs_directory() ->
    case file:make_dir("../logs") of
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

%% Function to store notification data in a log file
send_notification_to_log(PackageId, Status) ->
    %% Ensure the logs directory exists
    ensure_logs_directory(),

    %% Construct the JSON payload
    Payload = jsx:encode(#{package_id => PackageId, status => Status}),

    %% Define the log file path
    LogFile = "/Users/adamtyson/Desktop/CSE 481/tracker_business_logic/logs/notification_event.log",

    %% Append the JSON data to the log file
    case file:open(LogFile, [append]) of
        {ok, File} ->
            io:format(File, "~s~n", [Payload]),
            file:close(File),
            io:format("Notification data logged successfully for Package ~p with Status: ~p~n", [
                PackageId, Status
            ]),
            ok;
        {error, Reason} ->
            io:format("Failed to write notification data to log. Error: ~p~n", [Reason]),
            {error, Reason}
    end.

terminate(_Reason, _State) ->
    ok.

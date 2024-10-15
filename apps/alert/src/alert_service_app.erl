-module(alert_service_app).
-behaviour(gen_event).

%% API
-export([start_link/0, raise_alert/1, log_alert_to_file/3]).
-export([init/1, handle_event/2, terminate/2]).

%% Client API
start_link() ->
    gen_event:start_link({local, ?MODULE}).

%% Function to raise an alert (used by supervisors)
raise_alert(AlertMessage) ->
    gen_event:notify(?MODULE, {system_alert, AlertMessage}).

%% gen_event callbacks
init([]) ->
    {ok, #{}}.

%% Handle the alert event
handle_event({system_alert, Message}, State) ->
    io:format("ALERT RECEIVED: ~p~n", [Message]),
    %% Default logging function
    log_alert_to_file("log.txt", Message, fun file_log/2),
    {ok, State}.

%% Function to log alerts to a text file (with injected logging function for testability)
log_alert_to_file(FileName, Message, LogFunc) ->
    LogFunc(FileName, Message).

%% Default file logging function (used in production)
file_log(FileName, Message) ->
    {ok, File} = file:open(FileName, [append]),
    io:format(File, "ALERT: ~p - Timestamp: ~p~n", [Message, calendar:local_time()]),
    file:close(File).

terminate(_Reason, _State) ->
    ok.

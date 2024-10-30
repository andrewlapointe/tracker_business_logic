<<<<<<< Updated upstream
% Alert Service Using gen_event
% The Alert Service will act as a central point for processing alert events. Here’s the code for the service:

-module(alert_service).
-behaviour(gen_event).

%% API
-export([start_link/0, raise_alert/1]).
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
    %% Write the alert message to a log file
    log_alert_to_file("log.txt", Message),
    {ok, State}.

%% Function to log alerts to a text file
log_alert_to_file(FileName, Message) ->
    {ok, File} = file:open(FileName, [append]),
    io:format(File, "ALERT: ~p - Timestamp: ~p~n", [Message, calendar:local_time()]),
    file:close(File).

terminate(_Reason, _State) ->
    ok. 
=======
% Alert Service Using gen_event
% The Alert Service will act as a central point for processing alert events. Here’s the code for the service:

-module(alert_service).
-behaviour(gen_event).

%% API
-export([start_link/0, raise_alert/1]).
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
    %% Write the alert message to a log file
    log_alert_to_file("log.txt", Message),
    {ok, State}.

%% Function to log alerts to a text file
log_alert_to_file(FileName, Message) ->
    {ok, File} = file:open(FileName, [append]),
    io:format(File, "ALERT: ~p - Timestamp: ~p~n", [Message, calendar:local_time()]),
    file:close(File).

terminate(_Reason, _State) ->
    ok. 
>>>>>>> Stashed changes

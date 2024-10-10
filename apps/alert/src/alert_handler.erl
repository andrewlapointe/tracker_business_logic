% Alert Handler for Logging and Processing
% The Alert Handler is registered with the Alert Service and handles the incoming events, writing them to a log file:

-module(alert_handler).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, terminate/2]).

init([]) ->
    io:format("Alert handler started.~n"),
    {ok, #{}}.

%% Handle events from the Alert Service
handle_event({system_alert, {supervisor_failure, Reason, ChildSpec}}, State) ->
    io:format("Logging supervisor failure: ~p~n", [{Reason, ChildSpec}]),
    log_alert_to_file("log.txt", {Reason, ChildSpec}),
    {ok, State};

handle_event({system_alert, Message}, State) ->
    %% Handle generic alert messages
    io:format("Generic Alert Received: ~p~n", [Message]),
    log_alert_to_file("log.txt", Message),
    {ok, State}.

%% Function to log alerts to a text file
log_alert_to_file(FileName, Message) ->
    {ok, File} = file:open(FileName, [append]),
    io:format(File, "ALERT: ~p - Timestamp: ~p~n", [Message, calendar:local_time()]),
    file:close(File).

terminate(_Reason, State) ->
    io:format("Alert handler terminating.~n"),
    ok.
% Log Handler for Writing to a File
% This handler writes the time differences and events to a text file and can also handle updates to other services if needed:

-module(log_handler).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, terminate/2, log_event/2]).

init([]) ->
    io:format("Log handler started.~n"),
    {ok, #{}}.

%% Log the event if a time threshold is exceeded
log_event(PackageId, TimeDiff) ->
    gen_event:notify(?MODULE, {log_event, PackageId, TimeDiff}).

%% Handle the log event from the Analytics Service
handle_event({log_event, PackageId, TimeDiff}, State) ->
    io:format("Logging time difference for package ~p: ~p seconds~n", [PackageId, TimeDiff]),
    log_to_file("analytics_log.txt", PackageId, TimeDiff),
    {ok, State}.

%% Write to the log file
log_to_file(FileName, PackageId, TimeDiff) ->
    {ok, File} = file:open(FileName, [append]),
    io:format(File, "Package ~p: Time difference: ~p seconds - Timestamp: ~p~n", [PackageId, TimeDiff, calendar:local_time()]),
    file:close(File).

terminate(_Reason, State) ->
    io:format("Log handler terminating.~n"),
    ok.
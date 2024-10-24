% Notification Service Using gen_event
% Here’s the code for the Notification Service that aligns with the updated structure:

-module(notification_app).
-behaviour(gen_event).

%% API
-export([start_link/0, start_link/1, notify/2, notify/3, send_notification_via_https/2]).
-export([init/1, handle_event/2, terminate/2]).
% Why are we exporting notify/3? Do we expect different names to be passed in?

%% Client API
start_link() ->
    start_link(notification_service_event).

start_link(Name) ->
    gen_event:start_link({local, Name}).

notify(PackageId, Status) ->
    notify(notification_service_event, PackageId, Status).

notify(Name, PackageId, Status) ->
    gen_event:notify(Name, {package_update, PackageId, Status}).

%% gen_event callbacks
init([]) ->
    {ok, #{}}.

%% Handle the event for package updates
handle_event({package_update, PackageId, Status}, State) ->
    %% Here, handle the notification logic for a status change
    % io:format("Received update for package ~p with status: ~p~n", [PackageId, Status]),
    %% Call a function to send the notification via HTTPS (e.g., email or other)
    send_notification_via_https(PackageId, Status),
    {ok, State}.

%% Function to simulate sending a notification
send_notification_via_https(PackageId, Status) ->
    io:format("Sending notification for Package ~p with status ~p~n", [PackageId, Status]),
    ok.

terminate(_Reason, _State) ->
    ok.

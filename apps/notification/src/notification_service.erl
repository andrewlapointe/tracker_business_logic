% Notification Service Using gen_event
% Here’s the code for the Notification Service that aligns with the updated structure:

-module(notification_service).
-behaviour(gen_event).

%% API
-export([start_link/0, notify/2]).
-export([init/1, handle_event/2, terminate/2]).

%% Client API
start_link() ->
    gen_event:start_link({local, ?MODULE}).

%% Function to trigger notifications from the Package Monitor Service
notify(PackageId, Status) ->
    gen_event:notify(?MODULE, {package_update, PackageId, Status}).

%% gen_event callbacks
init([]) ->
    {ok, #{}}.

%% Handle the event for package updates
handle_event({package_update, PackageId, Status}, State) ->
    %% Here, handle the notification logic for a status change
    io:format("Received update for package ~p with status: ~p~n", [PackageId, Status]),
    %% Call a function to send the notification via HTTPS (e.g., email or other)
    send_notification_via_https(PackageId, Status),
    {ok, State}.

%% Function to simulate sending a notification
send_notification_via_https(PackageId, Status) ->
    io:format("Sending notification for Package ~p with status ~p~n", [PackageId, Status]),
    %% Actual logic to send notification would go here (e.g., call another service)
    ok.

terminate(_Reason, _State) ->
    ok.
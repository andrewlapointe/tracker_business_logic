-module(notification_event).
-behaviour(gen_event).

% ==================================
% TODO: This does not work. Needs to record notifications somewhere. Could be a text file or similar.
%       Does not need to be HTTPS as code implies.
% ==================================


%% API
-export([start_link/0, start_link/1, notify/2, notify/3, send_notification_via_https/2]).
-export([init/1, handle_event/2, terminate/2]).

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
    send_notification_via_https(PackageId, Status),
    {ok, State}.

%% Function to simulate sending a notification
send_notification_via_https(PackageId, Status) ->
    io:format("Sending notification for Package ~p with status ~p~n", [PackageId, Status]),
    ok.

terminate(_Reason, _State) ->
    ok.

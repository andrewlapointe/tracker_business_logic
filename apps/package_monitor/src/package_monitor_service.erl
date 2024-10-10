% Package Monitor Service Update
% The Package Monitor Service can trigger notifications by calling the notify/2 function from the Notification Service. Here’s how this part looks:

-module(package_monitor_service).
-behaviour(gen_event).

%% API
-export([start_link/0, notify_status_change/2]).
-export([init/1, handle_event/2, terminate/2]).

%% Client API
start_link() ->
    gen_event:start_link({local, ?MODULE}).

%% Function to notify about a status change
notify_status_change(PackageId, Status) ->
    notification_service:notify(PackageId, Status).

%% gen_event callbacks (for potential events related to monitoring)
init([]) ->
    {ok, #{}}.

handle_event(_Event, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok. 

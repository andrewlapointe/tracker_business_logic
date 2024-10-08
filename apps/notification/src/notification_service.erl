-module(notification_service).
-behaviour(gen_event).

%% API
-export([start_link/0, subscribe/1, notify/2]).
-export([init/1, handle_event/2, terminate/2]).

%% Client API
start_link() ->
    gen_event:start_link({local, ?MODULE}).

%% Subscribe a user for updates on their PackageId
subscribe(PackageId) ->
    gen_event:add_handler(?MODULE, {notification_handler, PackageId}, []).

%% Notify users when the status of a package changes
notify(PackageId, Status) ->
    gen_event:notify(?MODULE, {package_update, PackageId, Status}).

%% gen_event callbacks
init([]) ->
    {ok, #{}}.

%% Handle the event for package updates
handle_event({package_update, PackageId, Status}, State) ->
    %% Find subscribers for this PackageId and notify them
    io:format("Notifying users about update for ~p: Status: ~p~n", [PackageId, Status]),
    %% Send notification logic here (e.g., email or SMS)
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

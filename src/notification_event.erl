-module(notification_event).
-behaviour(gen_event).

%% API
-export([start_link/1, notify/3, send_notification_to_cowboy/2]).
-export([init/1, handle_event/2, terminate/2]).

%% Client API
start_link(?MODULE) ->
    gen_event:start_link({local, ?MODULE}).

notify(?MODULE, PackageId, Status) ->
    gen_event:notify(?MODULE, {package_update, PackageId, Status}).

%% gen_event callbacks
init([]) ->
    {ok, #{}}.

%% Handle the event for package updates
handle_event({package_update, PackageId, Status}, State) ->
    io:format("Handling event: PackageId=~p, Status=~p~n", [PackageId, Status]),
    send_notification_to_cowboy(PackageId, Status),
    {ok, State}.

%% Function to send a notification to the Cowboy handler
send_notification_to_cowboy(PackageId, Status) ->
    %% Define the Cowboy handler endpoint
    URL = "http://cowboy.mpc-us.com/notifications",

    %% Construct the JSON payload
    Payload = jsx:encode(#{package_id => PackageId, status => Status}),

    %% Define headers
    Headers = [{"Content-Type", "application/json"}],

    %% Perform the HTTP POST request
    case httpc:request(post, {URL, Headers, "application/json", Payload}, [], []) of
        {ok, {StatusCode, _, _}} when StatusCode >= 200, StatusCode < 300 ->
            io:format(
                "Notification sent successfully to Cowboy for Package ~p with Status: ~p~n", [
                    PackageId, Status
                ]
            ),
            ok;
        {ok, {StatusCode, _, Body}} ->
            io:format("Failed to send notification to Cowboy. HTTP Status: ~p. Response: ~p~n", [
                StatusCode, Body
            ]),
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            io:format("Failed to send notification to Cowboy. Error: ~p~n", [Reason]),
            {error, Reason}
    end.

terminate(_Reason, _State) ->
    ok.

-module(notification_handler).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, terminate/2]).

init(PackageId) ->
    io:format("Subscribed for updates on package: ~p~n", [PackageId]),
    {ok, PackageId}.

handle_event({package_update, PackageId, Status}, State) when PackageId =:= State ->
    io:format("Package ~p has a new status: ~p. Sending notification!~n", [PackageId, Status]),
    {ok, State};

handle_event(_, State) ->
    {ok, State}.

terminate(_Reason, State) ->
    io:format("Unsubscribed from notifications for package: ~p~n", [State]),
    ok.

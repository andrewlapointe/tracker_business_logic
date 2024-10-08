-module(package_monitor_service).
-behaviour(gen_server).

%% API
-export([start_link/0, update_location/2]).
-export([init/1, handle_cast/2, terminate/2, code_change/3]).

%% Client API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Update the package location or status
update_location(PackageId, LocationOrStatus) ->
    gen_server:cast(?MODULE, {update_location, PackageId, LocationOrStatus}).

%% gen_server callbacks
init([]) ->
    {ok, #{}}.

%% Handle update requests from external systems or APIs
handle_cast({update_location, PackageId, LocationOrStatus}, State) ->
    %% Update the package status in Riak (simulate with riak_kv:put)
    case riak_kv:put(PackageId, LocationOrStatus) of
        ok ->
            %% Notify the Notification Service about the status change
            notification_service:notify(PackageId, LocationOrStatus),
            io:format("Package ~p updated with status ~p.~n", [PackageId, LocationOrStatus]),
            {noreply, State};
        {error, Reason} ->
            io:format("Failed to update package ~p: ~p.~n", [PackageId, Reason]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

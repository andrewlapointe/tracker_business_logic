<<<<<<< Updated upstream
% Package Monitor Service Update
% The Package Monitor Service can trigger notifications by calling the notify/2 function from the Notification Service. Here’s how this part looks:

-module(package_monitor_service).
-behaviour(gen_server).

%% API
-export([start_link/0, update_db_record/2]).
-export([init/1, handle_cast/2, terminate/2, code_change/3]).

%% Client API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Update the database record with the provided data
update_db_record(PackageId, Data) ->
    gen_server:cast(?MODULE, {update_db_record, PackageId, Data}).

%% gen_server callbacks
init([]) ->
    {ok, #{}}.

handle_cast({update_db_record, PackageId, Data}, State) ->
    %% Store data in Riak using the package ID as the key
    case riak_kv:put(PackageId, Data) of
        ok ->
            %% Notify the Notification Service
            notification_service:notify(PackageId, Data),
            io:format("Package ~p updated successfully with data: ~p.~n", [PackageId, Data]),
            {noreply, State};
        {error, Reason} ->
            io:format("Failed to update package ~p: ~p.~n", [PackageId, Reason]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
=======
% Package Monitor Service Update
% The Package Monitor Service can trigger notifications by calling the notify/2 function from the Notification Service. Here’s how this part looks:

-module(package_monitor_service).
-behaviour(gen_server).

%% API
-export([start_link/0, update_db_record/2]).
-export([init/1, handle_cast/2, terminate/2, code_change/3]).

%% Client API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Update the database record with the provided data
update_db_record(PackageId, Data) ->
    gen_server:cast(?MODULE, {update_db_record, PackageId, Data}).

%% gen_server callbacks
init([]) ->
    {ok, #{}}.

handle_cast({update_db_record, PackageId, Data}, State) ->
    %% Store data in Riak using the package ID as the key
    case riak_kv:put(PackageId, Data) of
        ok ->
            %% Notify the Notification Service
            notification_service:notify(PackageId, Data),
            io:format("Package ~p updated successfully with data: ~p.~n", [PackageId, Data]),
            {noreply, State};
        {error, Reason} ->
            io:format("Failed to update package ~p: ~p.~n", [PackageId, Reason]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
>>>>>>> Stashed changes

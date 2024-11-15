% Package Monitor Service Update
% The Package Monitor Service can trigger notifications by calling the notify/2 function from the Notification Service. Here’s how this part looks:

-module(package_monitor_app).
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
    RiakHost = utils:riak_ip_address(),
    RiakPort = utils:port_number(),
    Bucket = <<"bucket">>,
    case riakc_pb_socket:start_link(RiakHost, RiakPort) of
        {ok, RiakPid} ->
            {ok, #{
                riak_pid => RiakPid,
                bucket => Bucket
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({update_db_record, PackageId, Data}, State) ->
    PackageId = maps:get(package_id, Data),
    case riakc_pb_socket:update(PackageId, Data) of
        ok ->
            %% Notify the Notification Service
            notification_app:notify(PackageId, Data),
            % io:format("Package ~p updated successfully with data: ~p.~n", [PackageId, Data]),
            {noreply, State};
        {error, Reason} ->
            io:format("Failed to update package ~p: ~p.~n", [PackageId, Reason]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

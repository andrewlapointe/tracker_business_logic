-module(package_monitor_server).
-behaviour(gen_server).

%% API
-export([start_link/0, update_db_record/2]).
-export([handle_info/2]).
-export([init/1, handle_cast/2, terminate/2, code_change/3]).

%% Client API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_db_record(PackageId, Data) ->
    gen_server:cast(?MODULE, {update_db_record, PackageId, Data}).

%% gen_server callbacks
init([]) ->
    RiakHost = utils:riak_ip_address(),
    RiakPort = utils:port_number(),
    Bucket = <<"bucket">>,
    %% Send a message to self to establish connection asynchronously
    self() ! {connect_riak, RiakHost, RiakPort},
    {ok, #{bucket => Bucket, riak_pid => undefined}}.

handle_cast({update_db_record, PackageId, Data}, State) ->
    RiakPid = maps:get(riak_pid, State),
    Bucket = maps:get(bucket, State),
    BinaryKey = integer_to_binary(PackageId),

    % Step 1: Fetch the current object
    case riakc_pb_socket:get(RiakPid, Bucket, BinaryKey) of
        {ok, Object} ->
            % Step 2: Update the object's value
            CurrentValue = riakc_obj:get_value(Object),
            case catch binary_to_term(CurrentValue) of
                Map when is_map(Map) ->
                    UpdatedMap = maps:merge(Map, Data), % Merge existing data with new data
                    UpdatedValue = term_to_binary(UpdatedMap),
                    UpdatedObject = riakc_obj:update_value(Object, UpdatedValue),

                    % Step 3: Write the updated object back to Riak
                    case riakc_pb_socket:put(RiakPid, UpdatedObject) of
                        ok ->
                            io:format("Package ~p updated successfully with data: ~p.~n", [PackageId, UpdatedMap]),
                            
                            %% Notify analytics_statem if the package is marked as delivered
                            case maps:get(status, Data, undefined) of
                                delivered ->
                                    CurrentTime = erlang:localtime(),
                                    case analytics_statem:update_package(PackageId, CurrentTime) of
                                        ok ->
                                            io:format("Analytics notified for delivered package ~p at ~p.~n", [PackageId, CurrentTime]);
                                        {error, Reason} ->
                                            io:format("Failed to notify analytics for package ~p: ~p.~n", [PackageId, Reason])
                                    end;
                                _ -> ok % No action for other statuses
                            end,

                            {noreply, State};
                        {error, Reason} ->
                            io:format("Failed to update package ~p: ~p.~n", [PackageId, Reason]),
                            {noreply, State}
                    end;
                _Error ->
                    io:format("Failed to decode existing data for package ~p.~n", [PackageId]),
                    {noreply, State}
            end;
        {error, notfound} ->
            io:format("Package ~p not found in the database.~n", [PackageId]),
            {noreply, State};
        {error, Reason} ->
            io:format("Error fetching package ~p: ~p.~n", [PackageId, Reason]),
            {noreply, State}
    end.

handle_info({connect_riak, RiakHost, RiakPort}, State) ->
    case riakc_pb_socket:start_link(RiakHost, RiakPort) of
        {ok, RiakPid} ->
            NewState = State#{riak_pid => RiakPid},
            {noreply, NewState};
        {error, Reason} ->
            %% Handle the connection error, possibly retry
            io:format("Failed to connect to Riak: ~p~n", [Reason]),
            %% Decide whether to retry, stop, or continue without Riak
            {stop, Reason, State}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

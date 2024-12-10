-module(package_monitor_server).
-behaviour(gen_server).

%% API
-export([start_link/0, update_db_record/2]).
-export([init/1, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Client API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_db_record(PackageId, BinaryData) ->
    %% Pass the raw binary data to the gen_server
    gen_server:cast(?MODULE, {update_db_record, PackageId, BinaryData}).

%% gen_server callbacks
init([]) ->
    RiakHost = utils:riak_ip_address(),
    RiakPort = utils:port_number(),
    Bucket = <<"bucket">>,
    %% Connect to Riak asynchronously
    self() ! {connect_riak, RiakHost, RiakPort},
    {ok, #{bucket => Bucket, riak_pid => undefined}}.

handle_cast({update_db_record, PackageId, BinaryData}, State) ->
    %% Parse the binary data
    case parse_package_data(BinaryData) of
        {ok, ParsedData} ->
            io:format("Parsed data for package ~p: ~p~n", [PackageId, ParsedData]),
            %% Fetch Riak connection details
            RiakPid = maps:get(riak_pid, State),
            Bucket = maps:get(bucket, State),
            BinaryKey = binary:copy(PackageId),

            %% Fetch the current object
            case riakc_pb_socket:get(RiakPid, Bucket, BinaryKey) of
                {ok, Object} ->
                    %% Update the object with new data
                    CurrentValue = riakc_obj:get_value(Object),
                    case catch binary_to_term(CurrentValue) of
                        Map when is_map(Map) ->
                            UpdatedMap = maps:merge(Map, ParsedData),
                            UpdatedValue = term_to_binary(UpdatedMap),
                            UpdatedObject = riakc_obj:update_value(Object, UpdatedValue),

                            %% Write the updated object back to Riak
                            case riakc_pb_socket:put(RiakPid, UpdatedObject) of
                                ok ->
                                    io:format("Package ~p updated successfully with data: ~p.~n", [PackageId, UpdatedMap]),
                                    %% Check for "Delivered" status and update analytics
                                    case maps:get(<<"status">>, UpdatedMap, <<"">>) of
                                        <<"Delivered">> ->
                                            io:format("Updating analytics for package ~p.~n", [PackageId]),
                                            analytics:update_package(<<"delivered_packages">>, 1),
                                            {noreply, State};
                                        _ ->
                                            {noreply, State}
                                    end;
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
            end;
        {error, Reason} ->
            io:format("Failed to parse data for package ~p: ~p.~n", [PackageId, Reason]),
            {noreply, State}
    end.

handle_info({connect_riak, RiakHost, RiakPort}, State) ->
    case riakc_pb_socket:start_link(RiakHost, RiakPort) of
        {ok, RiakPid} ->
            io:format("Successfully connected to Riak.~n"),
            NewState = State#{riak_pid => RiakPid},
            {noreply, NewState};
        {error, Reason} ->
            io:format("Failed to connect to Riak: ~p~n", [Reason]),
            {stop, Reason, State}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Function to parse package data
parse_package_data(BinaryData) ->
    try
        %% Convert binary to string
        StringData = binary_to_list(BinaryData),
        %% Replace '+' with space
        NormalizedData = lists:map(fun(Char) -> if Char =:= $+ -> $\s; true -> Char end end, StringData),
        %% Split by '&' into key-value pairs
        Pairs = string:tokens(NormalizedData, "&"),
        %% Parse each key-value pair into a map
        ParsedData = lists:foldl(fun parse_pair/2, #{}, Pairs),
        {ok, ParsedData}
    catch
        _:Error ->
            io:format("Failed to parse data.~n"),
            {error, invalid_data}
    end.

%% Parse a single key-value pair and add it to the map
parse_pair(Pair, Acc) ->
    case string:tokens(Pair, "=") of
        [Key, Value] ->
            DecodedKey = binary:copy(decode_url(Key)),
            DecodedValue = binary:copy(decode_url(Value)),
            maps:put(DecodedKey, DecodedValue, Acc);
        _ ->
            io:format("Skipping invalid pair: ~p~n", [Pair]),
            Acc
    end.

%% Decode URL-encoded values
decode_url(Value) ->
    try
        uri_string:decode(Value)
    catch
        _:_ -> binary:copy(Value) % Ensure binary output in case of failure
    end.

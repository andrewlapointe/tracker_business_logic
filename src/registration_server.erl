-module(registration_server).
-behaviour(gen_server).

%% API
-export([start_link/0, register_package/1]).
-export([handle_info/2]).
-export([init/1, handle_call/3, terminate/2, code_change/3]).

%% Client API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Function to handle registration data received as a map
register_package(PackageData) ->
    gen_server:call(?MODULE, {register, PackageData}).

%% gen_server callbacks
init([]) ->
    RiakHost = utils:riak_ip_address(),
    RiakPort = utils:port_number(),
    Bucket = <<"bucket">>,
    %% Send a message to self to establish connection asynchronously
    self() ! {connect_riak, RiakHost, RiakPort},
    {ok, #{bucket => Bucket, riak_pid => undefined}}.

handle_call({register_package, BinaryData}, _From, State) ->
    io:format("BinaryData received: ~p~n", [BinaryData]),
    case parse_package_data(BinaryData) of
        {ok, ParsedData} ->
            io:format("Parsed data: ~p~n", [ParsedData]),
            PackageKey = utils:generate_package_key(),
            io:format("Generated package key: ~p~n", [PackageKey]),
            {reply, {ok, "Package registered", PackageKey}, State};
        {error, Reason} ->
            io:format("Failed to parse data. Reason: ~p~n", [Reason]),
            {reply, {error, Reason}, State}
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

put(Key, Value, State) ->
    RiakPid = maps:get(riak_pid, State),
    Bucket = maps:get(bucket, State),
    BinaryKey = Key, % Already in binary format from generate_package_key/0
    Object = riakc_obj:new(Bucket, BinaryKey, Value),
    case riakc_pb_socket:put(RiakPid, Object) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

terminate(_Reason, State) ->
    RiakPid = maps:get(riak_pid, State),
    case RiakPid of
        undefined -> ok;
        _ -> riakc_pb_socket:stop(RiakPid)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

parse_package_data(BinaryData) ->
    try
        %% Convert binary to list (string)
        StringData = binary_to_list(BinaryData),
        %% Replace '+' with space
        NormalizedData = lists:map(fun(Char) -> if Char =:= $+ -> $\s; true -> Char end end, StringData),
        %% Split the string by '&' to get key-value pairs
        Pairs = string:tokens(NormalizedData, "&"),
        %% Parse each key-value pair into a map
        ParsedData = lists:foldl(fun parse_pair/2, #{}, Pairs),
        {ok, ParsedData}
    catch
        Class:Reason ->
            io:format("Parsing failed. Class: ~p, Reason: ~p~n", [Class, Reason]),
            {error, invalid_data}
    end.

parse_pair(Pair, Acc) ->
    case string:tokens(Pair, "=") of
        [Key, Value] ->
            DecodedKey = decode_url(Key),
            DecodedValue = decode_url(Value),
            maps:put(DecodedKey, DecodedValue, Acc);
        _ ->
            io:format("Skipping invalid pair: ~p~n", [Pair]),
            Acc
    end.


decode_url(Value) ->
    Normalized = lists:map(fun(Char) -> if Char =:= $+ -> $\s; true -> Char end end, Value),
    re:replace(Normalized, "%([0-9A-Fa-f]{2})", fun([Hex]) -> list_to_integer(Hex, 16) end, [global, {return, list}]).

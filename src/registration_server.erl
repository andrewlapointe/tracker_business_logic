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
    %% Process the binary data
    %% Example: Parse or decode the binary data if needed
    case parse_package_data(BinaryData) of
        {ok, ParsedData} ->
            %% Simulate storing the data or performing the required action
            PackageKey = utils:generate_package_key(),
            io:format("Package ~p registered with data: ~p~n", [PackageKey, ParsedData]),
            {reply, {ok, "Package registered", PackageKey}, State};
        {error, Reason} ->
            io:format("Failed to register package: ~p~n", [Reason]),
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
    %% Example: Decode URL-encoded binary data into a map
    try
        Decoded = http_uri:parse_query(binary_to_list(BinaryData)),
        {ok, maps:from_list(Decoded)}
    catch
        _:_ -> {error, invalid_data}
    end.
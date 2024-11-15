-module(registration_app).
-behaviour(gen_server).

%% API
-export([start_link/0, register_package/1]).
-export([init/1, handle_call/3, terminate/2, code_change/3]).

%% Record for storing package data
-record(package, {package_id, origin, destination, status}).

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
    case riakc_pb_socket:start_link(RiakHost, RiakPort) of
        {ok, RiakPid} ->
            {ok, #{
                riak_pid => RiakPid,
                bucket => Bucket
            }};
        {error, Reason} ->
            {stop, Reason}
    end.
    % {ok, #{}}.

handle_call({register, PackageData}, _From, State) ->
    %% Extract data from the map
    PackageId = maps:get(package_id, PackageData),
    %% Store in Riak (simulating here with riak_kv:put)
    case put(PackageId, PackageData, State) of
        {reply, ok, State} ->
            io:format("Package ~p registered successfully.~n", [PackageId]),
            {reply, {ok, "Package registered"}, State};
        {error, Reason} ->
            io:format("Failed to register package ~p: ~p~n", [PackageId, Reason]),
            {reply, {error, Reason}, State}
    end.


put(Key, Value, State) ->
    RiakPid = maps:get(riak_pid, State),
    Bucket = maps:get(bucket, State),
    BinaryKey = integer_to_binary(Key), 
    Object = riakc_obj:new(Bucket, BinaryKey, Value),
    case riakc_pb_socket:put(RiakPid, Object) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

terminate(_Reason, State) ->
    RiakPid = maps:get(riak_pid, State),
    riakc_pb_socket:stop(RiakPid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

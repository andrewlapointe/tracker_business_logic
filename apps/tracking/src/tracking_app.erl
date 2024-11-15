% Tracking Service (gen_server)
% The gen_server will handle requests for package information by querying Riak.

-module(tracking_app).
-behaviour(gen_server).

%% API
-export([start_link/0, get_status/1]).
-export([init/1, handle_call/3, terminate/2, code_change/3]).

%% Client API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Get the status of a package by its PackageId
get_status(PackageId) ->
    gen_server:call(?MODULE, {get_status, PackageId}).

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

handle_call({get_status, PackageId}, _From, State) ->
    %% Fetch the package status from the data_interaction module
    case get_package_status(PackageId, State) of
        {ok, Data} ->
            case catch binary_to_term(Data) of
                Term when is_map(Term) orelse is_list(Term) ->
                    {reply, {ok, Term}, State};
                _Error ->
                    {reply, {error, "Unable to decode data"}, State}
            end;
        {error, not_found} ->
            {reply, {error, "Package not found"}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

get_package_status(PackageId, State) ->
    RiakPid = maps:get(riak_pid, State),
    Bucket = maps:get(bucket, State),
    BinaryKey = integer_to_binary(PackageId),
    case riakc_pb_socket:get(RiakPid, Bucket, BinaryKey) of
        {ok, Object} ->
            {ok, riakc_obj:get_value(Object)};
        {error, Reason} ->
            {error, Reason}
    end.

terminate(_Reason, State) ->
    RiakPid = maps:get(riak_pid, State),
    riakc_pb_socket:stop(RiakPid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


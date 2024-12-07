-module(tracking_server).
-behaviour(gen_server).

%% API
-export([start_link/0, get_status/1]).
-export([handle_info/2]).
-export([init/1, handle_call/3, terminate/2, code_change/3]).

%% Client API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_status(PackageId) ->
    gen_server:call(?MODULE, {get_status, PackageId}).

%% gen_server callbacks
init([]) ->
    RiakHost = utils:riak_ip_address(),
    RiakPort = utils:port_number(),
    Bucket = <<"bucket">>,
    %% Send a message to self to establish connection asynchronously
    self() ! {connect_riak, RiakHost, RiakPort},
    {ok, #{bucket => Bucket, riak_pid => undefined}}.

handle_call({get_status, PackageId}, _From, State) ->
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

get_package_status(PackageId, State) ->
    RiakPid = maps:get(riak_pid, State),
    Bucket = maps:get(bucket, State),
    case riakc_pb_socket:get(RiakPid, Bucket, PackageId) of
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


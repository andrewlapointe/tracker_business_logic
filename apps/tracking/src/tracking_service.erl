-module(tracking_service).
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
    {ok, #{}}.

handle_call({get_status, PackageId}, _From, State) ->
    %% Fetch package status from Riak
    case riak_kv:get(PackageId) of
        {ok, Status} ->
            {reply, {ok, Status}, State};
        {error, not_found} ->
            {reply, {error, "Package not found"}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

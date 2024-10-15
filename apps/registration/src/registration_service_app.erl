-module(registration_service_app).
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
    {ok, #{}}.

handle_call({register, PackageData}, _From, State) ->
    %% Extract data from the map
    PackageId = maps:get(package_id, PackageData),
    %% Store in Riak (simulating here with riak_kv:put)
    case riak_kv:put(PackageId, PackageData) of
        ok ->
            io:format("Package ~p registered successfully.~n", [PackageId]),
            {reply, {ok, "Package registered"}, State};
        {error, Reason} ->
            io:format("Failed to register package ~p: ~p~n", [PackageId, Reason]),
            {reply, {error, Reason}, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%% @doc analytics top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(analytics_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, init/1, start_analytics/1]).

%% Starting the supervisor
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
<<<<<<< Updated upstream
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
=======
    SupFlags = #{strategy => simple_one_for_one},
    %% Supervisor child spec
ChildSpec = #{id => analytics_service,
              start => {analytics_service, start_link, []},
              restart => temporary,
              shutdown => brutal_kill,
              type => worker},
    {ok, {SupFlags, [ChildSpec]}}.
>>>>>>> Stashed changes

%% Starts an analytics process for a specific package
start_analytics(PackageId) ->
    supervisor:start_child(?MODULE, [PackageId]).
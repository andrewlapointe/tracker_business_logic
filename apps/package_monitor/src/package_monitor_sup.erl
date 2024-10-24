%%%-------------------------------------------------------------------
%% @doc package_monitor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(package_monitor_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 3,
                 period => 5},
    ChildSpecs = [
                #{id => package_monitor_app,            % Unique ID for this child
          start => {package_monitor_app, start_link, []}, % Start function
          restart => permanent,                 % Always restart on failure
          shutdown => 5000,                     % 5-second timeout for shutdown
          type => worker,                       % Type: worker process
          modules => [package_monitor_app]}     % Modules this child uses
        ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

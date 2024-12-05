%%%-------------------------------------------------------------------
%% @doc package_tracker top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(package_tracker_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        child(tracking_server, worker),
        child(registration_server, worker),
        child(package_monitor_server, worker),
        child(alert_event, worker)],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
child(Module,Type)->
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
	#{id => Module,
	  start => {Module,start_link,[]},
	  restart => permanent,
	  shutdown => 2000,
	  type => Type,
	  modules => [Module]}.
    
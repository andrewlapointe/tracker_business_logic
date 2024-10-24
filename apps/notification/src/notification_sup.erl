%%%-------------------------------------------------------------------
%% @doc notification top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(notification_sup).

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
                #{id => notification_app,            % Unique ID for this child
          start => {notification_app, start_link, []}, % Start function
          restart => permanent,                 % Always restart on failure
          shutdown => 5000,                     % 5-second timeout for shutdown
          type => worker,                       % Type: worker process
          modules => [notification_app]}     % Modules this child uses
        ],

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

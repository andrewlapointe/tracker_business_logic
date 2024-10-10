% Supervisor Integration with the Alert Service
% The supervisors should notify the Alert Service when one of their child processes fails. Here’s how you can set up a supervisor to do this:

-module(example_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Define the supervised children
    Children = [
        {some_worker, {some_worker_module, start_link, []}, permanent, 5000, worker, [some_worker_module]}
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.

%% Handle supervisor termination events and notify alert_service
terminate_child(Reason, ChildSpec) ->
    io:format("Supervisor detected failure: ~p. Notifying alert service.~n", [Reason]),
    alert_service:raise_alert({supervisor_failure, Reason, ChildSpec}).
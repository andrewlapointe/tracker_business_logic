-module(pm_db).
-export([put/2]).

%% Dummy implementation of riak_kv:put/2
put(_Key, _Value) ->
    ok.

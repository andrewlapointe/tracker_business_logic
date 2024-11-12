-module(registration_db).
-export([put/3]).

%% Dummy implementation of riak_kv:put/2
put(Key, Value, State) ->
    RiakPid = maps:get(riak_pid, State),
    Bucket = maps:get(bucket, State),
    Object = riakc_obj:new(Bucket, Key, Value),
    case riakc_pb_socket:put(RiakPid, Object) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.
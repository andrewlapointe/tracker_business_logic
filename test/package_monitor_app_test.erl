-module(package_monitor_app_test).
-include_lib("eunit/include/eunit.hrl").

%% Tests
update_db_record_test() ->

    %% Suppress logging
    logger:set_handler_config(default, level, none),
    
    %% Mock riakc_pb_socket
    meck:new(riakc_pb_socket, [unstick]),

    %% Simulate a successful `get` call
    meck:expect(riakc_pb_socket, get, fun(_RiakPid, _Bucket, <<"123">>) ->
        {ok, riakc_obj:new(<<"bucket">>, <<"123">>, term_to_binary(#{existing_key => "value"}))}
    end),

    %% Simulate a successful `put` call
    meck:expect(riakc_pb_socket, put, fun(_RiakPid, _UpdatedObject) -> ok end),

    %% Mock io:format to prevent real logging
    meck:new(io, [unstick]),
    meck:expect(io, format, fun(_Fmt, _Args) -> ok end),

    %% Start the package monitor server
    package_monitor_server:start_link(),

    %% Test the update_db_record functionality
    package_monitor_server:update_db_record("123", #{new_key => "new_value"}),

    %% Verify interactions
    ?_assert(meck:called(riakc_pb_socket:get('_', <<"bucket">>, <<"123">>))),
    ?_assert(meck:called(riakc_pb_socket:put('_', '_'))),
    ?_assert(meck:called(io:format('_', '_'))),

    %% Clean up mocks
    meck:unload(riakc_pb_socket),
    meck:unload(io),

    ok.

update_db_record_not_found_test() ->
    %% Mock riakc_pb_socket
    meck:new(riakc_pb_socket, [unstick]),

    %% Simulate a not-found case for `get`
    meck:expect(riakc_pb_socket, get, fun(_RiakPid, _Bucket, <<"456">>) -> {error, notfound} end),

    %% Mock io:format to prevent real logging
    meck:new(io, [unstick]),
    meck:expect(io, format, fun(_Fmt, _Args) -> ok end),

    %% Start the package monitor server
    package_monitor_server:start_link(),

    %% Test the update_db_record functionality
    package_monitor_server:update_db_record("456", #{key => "value"}),

    %% Verify interactions
    ?_assert(meck:called(riakc_pb_socket:get('_', <<"bucket">>, <<"456">>))),
    ?_assert(meck:called(io:format('_', '_'))),

    %% Clean up mocks
    meck:unload(riakc_pb_socket),
    meck:unload(io),

    ok.

update_db_record_error_test() ->
    %% Mock riakc_pb_socket
    meck:new(riakc_pb_socket, [unstick]),

    %% Simulate an error for `get`
    meck:expect(riakc_pb_socket, get, fun(_RiakPid, _Bucket, <<"789">>) -> {error, some_error} end),

    %% Mock io:format to prevent real logging
    meck:new(io, [unstick]),
    meck:expect(io, format, fun(_Fmt, _Args) -> ok end),

    %% Start the package monitor server
    package_monitor_server:start_link(),

    %% Test the update_db_record functionality
    package_monitor_server:update_db_record("789", #{key => "value"}),

    %% Verify interactions
    ?_assert(meck:called(riakc_pb_socket:get('_', <<"bucket">>, <<"789">>))),
    ?_assert(meck:called(io:format('_', '_'))),

    %% Clean up mocks
    meck:unload(riakc_pb_socket),
    meck:unload(io),

    ok.

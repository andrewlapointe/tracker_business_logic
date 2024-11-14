-module(utils).
-export([riak_ip_address/0, port_number/0, generate_package_key/0]).

riak_ip_address() ->
    % Change this to match the ip address of your riak droplet
    "146.190.175.63".

port_number() ->
    8087.

generate_package_key() ->
    UniqueID = erlang:unique_integer([monotonic, positive]),
    Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Key = io_lib:format("pkg-~p", [Timestamp + UniqueID]),
    list_to_binary(Key).
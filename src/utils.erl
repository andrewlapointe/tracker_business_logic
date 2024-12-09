-module(utils).
-export([riak_ip_address/0, port_number/0, generate_package_key/0]).

riak_ip_address() ->
    case os:getenv("RIAK_IP_ADDRESS") of
        undefined ->
            % Default IP address if the environmental variable is not set
            "24.144.84.21";
        IP ->
            IP
    end.

port_number() ->
    case os:getenv("RIAK_PORT") of
        undefined ->
            % Default port number if the environmental variable is not set
            8087;
        PortString ->
            % Convert the string to an integer
            list_to_integer(PortString)
    end.

generate_package_key() ->
    UniqueID = erlang:unique_integer([monotonic, positive]),
    Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Key = io_lib:format("pkg-~p", [Timestamp + UniqueID]),
    list_to_binary(Key).

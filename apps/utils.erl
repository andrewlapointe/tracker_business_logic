-module(utils).
-export([riak_ip_address/0, port_number/0]).

riak_ip_address() ->
    % Change this to match the ip address of your riak droplet
    "146.190.175.63".

port_number() ->
    "".
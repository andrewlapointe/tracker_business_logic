% Starting the Cowboy HTTP Server
% Set up Cowboy to use the handler and start the HTTPS listener:

-module(trackinghttp_server).
-export([start_link/0]).

start_link() ->
    %% Define routes
    Dispatch = cowboy_router:compile([
        {'', [
            {"/status", trackinghttp_handler, []}
        ]}
    ]),

    %% Define options for the server (including SSL configuration if needed)
    {ok} = cowboy:start_https(
        my_https_listener, 
        %% SSL Options (use your SSL certificate paths here)
        #{port => 8443, certfile => "/path/to/cert.pem", keyfile => "/path/to/key.pem"}, 
        #{env => #{dispatch => Dispatch}}
    ),

    io:format("HTTPS server started on port 8443~n"),
    {ok, self()}.
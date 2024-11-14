% Starting the Cowboy HTTP Server
% To set up the Cowboy HTTP server to use this handler, create a simple module that starts Cowboy and listens for HTTPS connections:


-module(registration_http_server).
-export([start_link/0]).

start_link() ->
    %% Define routes
    Dispatch = cowboy_router:compile([
        {'', [
            {"/register", registration_http_handler, []}
        ]}
    ]),

    %% Define options for the server (including SSL configuration if needed)
    {ok} = cowboy:start_https(
        my_https_listener, 
        %% SSL Options (certificates would go here for a real HTTPS setup)
        #{port => 8443, certfile => "/path/to/cert.pem", keyfile => "/path/to/key.pem"}, 
        #{env => #{dispatch => Dispatch}}
    ),

    io:format("HTTPS server started on port 8443~n"),
    {ok, self()}.
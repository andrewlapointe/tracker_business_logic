% HTTP Handler for Notifications
% An HTTPS handler can be included if there’s a need to receive incoming requests related to notifications, like managing user subscriptions or querying current notification preferences. Here’s an example handler:

-module(notificationhttp_handler).
-behaviour(cowboy_http).

%% API
-export([init/2, handle/2]).

init(Req, Opts) ->
    {ok, Req, Opts}.

handle(Req, State) ->
    %% Extract the method and path from the request
    Method = cowboyreq:method(Req),
    Path = cowboy_req:path(Req),

    %% Handle requests related to notifications
    case {Method, Path} of
        {post, "/subscribe"} ->
            %% Process subscription (for simplicity, we'll just print a message)
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            io:format("Received subscription request: ~p~n", [Body]),
            {ok, Req3} = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, <<"Subscription accepted">>, Req2),
            {ok, Req3, State};
        _->
            %% Respond with 404 Not Found for other methods/paths
            {ok, Req3} = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, <<"Not Found">>, Req),
            {ok, Req3, State}
    end.
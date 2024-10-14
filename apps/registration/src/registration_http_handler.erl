% HTTP Handler (Using Cowboy)
% The HTTP handler will accept POST requests containing JSON data and send this data to the Registration Service.

-module(registration_http_handler).
-behaviour(cowboy_http).

%% API
-export([init/2, handle/2]).

init(Req, Opts) ->
    {ok, Req, Opts}.

%% Handle incoming HTTP requests
handle(Req, State) ->
    %% Extract the method and path from the request
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    %% Only handle POST requests to "/register"
    case {Method, Path} of
        {post, "/register"} ->
            %% Read the JSON body from the request
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            %% Decode the JSON body into a map
            case jsx:decode(Body, [return_maps]) of
                {ok, JsonMap} ->
                    %% Pass the parsed JSON to the registration service
                    case registration_service:register_package(JsonMap) of
                        {ok, Msg} ->
                            %% Respond with 201 Created
                            {ok, Req3} = cowboy_req:reply(201, #{<<"content-type">> => <<"application/json">>}, jsx:encode(#{message => Msg}), Req2),
                            {ok, Req3, State};
                        {error, Reason} ->
                            %% Respond with 500 Internal Server Error
                            {ok, Req3} = cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, jsx:encode(#{error => Reason}), Req2),
                            {ok, Req3, State}
                    end;
                {error, _Reason} ->
                    %% Respond with 400 Bad Request if JSON parsing fails
                    {ok, Req3} = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"Invalid JSON">>, Req2),
                    {ok, Req3, State}
            end;
        _ ->
            %% Respond with 404 Not Found for other methods/paths
            {ok, Req3} = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, <<"Not Found">>, Req),
            {ok, Req3, State}
    end.
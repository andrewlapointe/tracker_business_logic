% HTTP Handler (Using Cowboy)
% The HTTP handler will accept POST requests containing JSON data and send this data to the Registration Service.

-module(registration_http_handler).
-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),

    %% Only handle POST requests to "/register"
    Response = case {Method, Path} of
        {post, "/register"} ->
            %% Read the JSON body
            case cowboy_req:read_body(Req0) of
                {ok, Body, Req1} ->
                    %% Decode JSON into a map
                    case jsx:decode(Body, [return_maps]) of
                        {ok, JsonMap} ->
                            %% Call registration_app to add the package
                            case registration_app:register_package(JsonMap) of
                                {ok, Msg} ->
                                    cowboy_req:reply(201, #{<<"content-type">> => <<"application/json">>}, jsx:encode(#{message => Msg}), Req1);
                                {error, Reason} ->
                                    cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, jsx:encode(#{error => Reason}), Req1)
                            end;
                        {error, DecodeReason} ->
                            io:format("JSON decode error: ~p~n", [DecodeReason]),
                            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"Invalid JSON">>, Req1)
                    end;
                {error, ReadError} ->
                    io:format("Request body read error: ~p~n", [ReadError]),
                    cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"Failed to read request body">>, Req0)
            end;
        _ ->
            %% Respond with 404 Not Found for other methods/paths
            cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, <<"Not Found">>, Req0)
    end,

    {ok, Response, Opts}.

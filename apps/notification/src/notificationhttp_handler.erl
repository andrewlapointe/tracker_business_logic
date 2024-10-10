% HTTPS Handler (Using Cowboy)
% This handler accepts GET requests with a packageid query parameter and retrieves the corresponding package data from the Tracking Service.

% -module(tracking_http_handler).
% -behaviour(cowboy_http).

% %% API
% -export([init/2, handle/2]).

% %% Include a JSON parsing library
% -include_lib("jsx/include/jsx.hrl").

% init(Req, Opts) ->
%     {ok, Req, Opts}.

% handle(Req, State) ->
%     %% Extract the method and path from the request
%     Method = cowboyreq:method(Req),
%     Path = cowboy_req:path(Req),

%     %% Only handle GET requests to "/status"
%     case {Method, Path} of
%         {get, "/status"} ->
%             %% Extract the "package_id" from the query string
%             {ok, PackageId, Req2} = cowboy_req:qs_val(<<"package_id">>, Req),
%             %% Call the Tracking Service to get the status
%             case tracking_service:get_status(PackageId) of
%                 {ok, Data} ->
%                     %% Convert the data map to JSON and respond with 200 OK
%                     JsonResponse = jsx:encode(Data),
%                     {ok, Req3} = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, JsonResponse, Req2),
%                     {ok, Req3, State};
%                 {error, Reason} ->
%                     %% Respond with 404 or 500 based on the error
%                     Status = case Reason of
%                         "Package not found" -> 404;
%                         "Internal Service Error" -> 500
%                     end,
%                     {ok, Req3} = cowboyreq:reply(Status, #{<<"content-type">> => <<"application/json">>}, jsx:encode(#{error => Reason}), Req2),
%                     {ok, Req3, State}
%             end; 
%         _->
%             %% Respond with 404 Not Found for other methods/paths
%             {ok, Req3} = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, <<"Not Found">>, Req),
%             {ok, Req3, State}
%     end.

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
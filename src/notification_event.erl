-module(notification_event).
-behaviour(gen_event).

% ==================================
% TODO: This does not work. Needs to record notifications somewhere. Could be a text file or similar.
%       Does not need to be HTTPS as code implies.
% ==================================

%% API
-export([start_link/0, start_link/1, notify/2, notify/3, send_notification_via_https/2]).
-export([init/1, handle_event/2, terminate/2]).

%% Client API
start_link() ->
    start_link(notification_service_event).

start_link(Name) ->
    gen_event:start_link({local, Name}).

notify(PackageId, Status) ->
    notify(notification_service_event, PackageId, Status).

notify(Name, PackageId, Status) ->
    gen_event:notify(Name, {package_update, PackageId, Status}).

%% gen_event callbacks
init([]) ->
    {ok, #{}}.

%% Handle the event for package updates
handle_event({package_update, PackageId, Status}, State) ->
    io:format("Handling event: PackageId=~p, Status=~p~n", [PackageId, Status]),
    record_notification(PackageId, Status),
    {ok, State}.

%% Function to simulate sending a notification
send_notification_via_https(PackageId, Status) ->
    %% Define the HTTPS endpoint
    URL = "https://cowboy.mpc-us.com/",

    %% Construct the JSON payload
    Payload = jsx:encode(#{package_id => PackageId, status => Status}),

    %% Define headers
    Headers = [{"Content-Type", "application/json"}],

    %% Define HTTP request options
    Options = [{ssl, [{verify, verify_peer}, {cacertfile, "/etc/ssl/certs/ca-certificates.crt"}]}],

    %% Make the POST request
    case httpc:request(post, {URL, Headers, "application/json", Payload}, Options, []) of
        {ok, {StatusCode, _, _}} when StatusCode >= 200, StatusCode < 300 ->
            io:format("Notification sent successfully for Package ~p with Status: ~p~n", [
                PackageId, Status
            ]),
            ok;
        {ok, {StatusCode, _, Body}} ->
            io:format("Failed to send notification. HTTP Status: ~p. Response: ~p~n", [
                StatusCode, Body
            ]),
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            io:format("Failed to send notification. Error: ~p~n", [Reason]),
            {error, Reason}
    end.

terminate(_Reason, _State) ->
    ok.

record_notification(PackageId, Status) ->
    File = "/home/atyson/tracker_business_logic/notifications.log",
    case filelib:is_dir("/home/atyson/tracker_business_logic/") of
        true ->
            write_to_file(File, PackageId, Status);
        false ->
            io:format("Directory does not exist. Creating directory...~n"),
            case file:make_dir("/home/atyson/tracker_business_logic/") of
                ok ->
                    write_to_file(File, PackageId, Status);
                {error, Reason} ->
                    io:format("Failed to create directory: ~p~n", [Reason]),
                    {error, Reason}
            end
    end.

write_to_file(File, PackageId, Status) ->
    Entry = io_lib:format("Package ~p: ~p~n", [PackageId, Status]),
    io:format("Attempting to write to file: ~p~n", [File]),
    io:format("Log entry: ~s~n", [Entry]),
    case file:write_file(File, Entry, [append]) of
        ok ->
            io:format("File write successful: ~p~n", [File]),
            ok;
        {error, Reason} ->
            io:format("File write failed: ~p~n", [Reason]),
            {error, Reason}
    end.

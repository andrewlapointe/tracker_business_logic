-module(analytics_statem_test).
-include_lib("eunit/include/eunit.hrl").

%% Tests
process_alive_test() ->
    {ok, Pid} = analytics_statem:start_link(),
    try
        ?assert(is_process_alive(Pid))
    after
        gen_statem:stop(Pid)
    end.

register_package_test() ->
    {ok, Pid} = analytics_statem:start_link(),
    try
        ?assertEqual(ok, analytics_statem:track_package(1, {10, 0, 0}))
    after
        gen_statem:stop(Pid)
    end.

package_out_test() ->
    {ok, Pid} = analytics_statem:start_link(),
    try
        ?assertEqual(ok, analytics_statem:track_package(1, {10, 0, 0})),
        ?assertEqual(ok, analytics_statem:update_package(1, {10, 20, 0}))
    after
        gen_statem:stop(Pid)
    end.

file_creation_test() ->
    UniqueFileName = io_lib:format("package_logs_~p.txt", [self()]),
    file:delete(UniqueFileName),
    {ok, Pid} = analytics_statem:start_link(),
    try
        %% Simulate package actions
        analytics_statem:track_package(1, {10, 0, 0}),
        analytics_statem:update_package(1, {10, 21, 0}), %% Use a time diff > 20 minutes

        %% Poll for file existence
        ?assertEqual(ok, wait_for_file(UniqueFileName, 1000)),
        
        %% Validate log content
        {ok, FileContent} = file:read_file(UniqueFileName),
            io:format("Raw file content: ~p~n", [FileContent]),
        CharCodes = binary:bin_to_list(FileContent),
        io:format("Character codes: ~p~n", [CharCodes]),
        io:format("Content as string (visualized): ~s~n", [lists:map(fun(X) -> if X >= 32, X =< 126 -> X; true -> $. end end, CharCodes)])

    after
        file:delete(UniqueFileName),
        gen_statem:stop(Pid)
    end.

%% Helper: Poll for file existence
wait_for_file(FileName, Timeout) ->
    case file:read_file(FileName) of
        {ok, _} -> ok;
        _ when Timeout > 0 ->
            timer:sleep(10),
            wait_for_file(FileName, Timeout - 10);
        _ -> error
    end.

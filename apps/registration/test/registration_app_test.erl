-module(registration_app_test).
-include_lib("eunit/include/eunit.hrl").


%% Test function for registering a package
register_package_test_() ->
    %% Define the test cases
    {
        setup,
        fun setup/0,
        fun cleanup/1,
        [
            %% Successful registration
            ?_test(register_package_success_test()),
            %% Failed registration
            ?_test(register_package_failure_test())
        ]
    }.

%% Setup function to start the gen_server
setup() ->
    %% Ensure the registration_app is started correctly
    case registration_app:start_link() of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.
%% Cleanup function (if needed)
cleanup(_) ->
    ok.

%% Test for successful package registration
register_package_success_test() ->
    %% Define mock data
    % PackageData = #{package_id => <<"PKG123">>, origin => <<"Origin">>, destination => <<"Destination">>, status => <<"pending">>},
    PackageID = utils:generate_package_key(),
    PackageData = #{package_id => PackageID , origin => <<"Origin">>, destination => <<"Destination">>, status => <<"pending">>},

    %% Mock riak_kv:put/2 to return 'ok'
    meck:new(registration_db),
    meck:expect(registration_db, put, fun(_PackageId, _PackageData) -> ok end),

    %% Call the registration function
    {ok, "Package registered"} = registration_app:register_package(PackageData),

    %% Check that the function was called correctly
    % ?assert(meck:called(registration_db, put, [<<"PKG123">>, PackageData])),
    ?assert(meck:called(registration_db, put, [PackageID, PackageData])),

    %% Unload the mock
    meck:unload(),
    ok.

register_package_failure_test() ->
    %% Define mock data
    % PackageData = #{package_id => <<"PKG123">>, origin => <<"Origin">>, destination => <<"Destination">>, status => <<"pending">>},
    PackageID = utils:generate_package_key(),
    PackageData = #{package_id => PackageID, origin => <<"Origin">>, destination => <<"Destination">>, status => <<"pending">>},

    %% Mock riak_kv:put/2 to return an error
    meck:new(registration_db),
    meck:expect(registration_db, put, fun(_PackageId, _PackageData) -> {error, <<"Some error">>} end),

    %% Call the registration function
    {error, <<"Some error">>} = registration_app:register_package(PackageData),

    %% Check that the function was called correctly
    ?assert(meck:called(registration_db, put, [PackageID, PackageData])),

    %% Unload the mock
    meck:unload(),
    ok.

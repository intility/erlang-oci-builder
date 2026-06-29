-module(ocibuild_shutdown_tests).
-moduledoc "Tests that HTTP and SSL shutdown leaves no orphaned processes.".

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests for ocibuild_http:stop/0
%%%===================================================================

stop_when_not_started_test() ->
    ?assertEqual(ok, ocibuild_http:stop()).

stop_kills_supervisor_test() ->
    ok = ocibuild_http:start(),
    ?assertNotEqual(undefined, whereis(ocibuild_http_sup)),
    ok = ocibuild_http:stop(),
    ?assertEqual(undefined, whereis(ocibuild_http_sup)).

stop_kills_pool_test() ->
    ok = ocibuild_http:start(),
    ?assertNotEqual(undefined, whereis(ocibuild_http_pool)),
    ok = ocibuild_http:stop(),
    ?assertEqual(undefined, whereis(ocibuild_http_pool)).

stop_is_idempotent_test() ->
    ok = ocibuild_http:start(),
    ok = ocibuild_http:stop(),
    ok = ocibuild_http:stop().

%%%===================================================================
%%% Tests for ocibuild_registry:stop_httpc/0
%%%===================================================================

stop_httpc_does_not_stop_ssl_test() ->
    %% stop_httpc must NOT stop SSL or its dependencies (crypto, asn1, public_key).
    %% Stopping applications is unsafe when ocibuild is used as a library inside
    %% another app that depends on them.
    ssl:start(),
    ocibuild_registry:stop_httpc(),
    %% SSL should still be running
    ?assert(lists:keymember(ssl, 1, application:which_applications())),
    ?assert(lists:keymember(crypto, 1, application:which_applications())),
    %% Clean up for other tests
    ssl:stop().

stop_httpc_when_nothing_running_test() ->
    ?assertEqual(ok, ocibuild_registry:stop_httpc()).

stop_httpc_stops_supervisor_test() ->
    ok = ocibuild_http:start(),
    ?assertNotEqual(undefined, whereis(ocibuild_http_sup)),
    ocibuild_registry:stop_httpc(),
    ?assertEqual(undefined, whereis(ocibuild_http_sup)).

stop_httpc_kills_orphaned_processes_test() ->
    %% Simulate orphaned httpc processes (e.g., from a crashed worker)
    Orphan1 = spawn(fun() -> receive stop -> ok end end),
    register(httpc_ocibuild_orphan1, Orphan1),
    Orphan2 = spawn(fun() -> receive stop -> ok end end),
    register(httpc_ocibuild_orphan2, Orphan2),
    ?assert(is_process_alive(Orphan1)),
    ?assert(is_process_alive(Orphan2)),
    ocibuild_registry:stop_httpc(),
    timer:sleep(10),
    ?assertNot(is_process_alive(Orphan1)),
    ?assertNot(is_process_alive(Orphan2)),
    ?assertEqual(undefined, whereis(httpc_ocibuild_orphan1)),
    ?assertEqual(undefined, whereis(httpc_ocibuild_orphan2)).

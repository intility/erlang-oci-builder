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

stop_httpc_stops_ssl_test() ->
    ssl:start(),
    ?assertMatch({ok, _}, application:ensure_all_started(ssl)),
    ocibuild_registry:stop_httpc(),
    ?assertEqual({error, {not_started, ssl}}, ssl:stop()).

stop_httpc_when_nothing_running_test() ->
    ?assertEqual(ok, ocibuild_registry:stop_httpc()).

stop_httpc_stops_supervisor_test() ->
    ok = ocibuild_http:start(),
    ?assertNotEqual(undefined, whereis(ocibuild_http_sup)),
    ocibuild_registry:stop_httpc(),
    ?assertEqual(undefined, whereis(ocibuild_http_sup)).

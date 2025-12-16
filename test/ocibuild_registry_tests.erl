%%%-------------------------------------------------------------------
-module(ocibuild_registry_tests).
-moduledoc "Tests for OCI registry client with mocked HTTP".

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test fixtures
%%%===================================================================

%% Sample manifest JSON
sample_manifest() ->
    ocibuild_json:encode(#{
        ~"schemaVersion" => 2,
        ~"mediaType" => ~"application/vnd.oci.image.manifest.v1+json",
        ~"config" => #{
            ~"mediaType" => ~"application/vnd.oci.image.config.v1+json",
            ~"digest" => ~"sha256:abc123",
            ~"size" => 1234
        },
        ~"layers" => []
    }).

%% Sample config JSON
sample_config() ->
    ocibuild_json:encode(#{
        ~"architecture" => ~"amd64",
        ~"os" => ~"linux",
        ~"config" => #{},
        ~"rootfs" => #{~"type" => ~"layers", ~"diff_ids" => []}
    }).

%%%===================================================================
%%% Setup / Teardown
%%%===================================================================

setup() ->
    meck:new(httpc, [unstick, passthrough]),
    ok.

cleanup(_) ->
    meck:unload(httpc).

%%%===================================================================
%%% Test generators
%%%===================================================================

registry_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        {"pull manifest from generic registry", fun pull_manifest_generic_test/0},
        {"pull blob", fun pull_blob_test/0},
        {"check blob exists returns true", fun check_blob_exists_true_test/0},
        {"check blob exists returns false", fun check_blob_exists_false_test/0},
        {"http error handling", fun http_error_test/0}
    ]}.

%%%===================================================================
%%% Pull manifest tests
%%%===================================================================

pull_manifest_generic_test() ->
    %% Mock HTTP responses for a generic registry (not docker.io)
    meck:expect(httpc, request, fun
        (get, {"https://ghcr.io/v2/myorg/myapp/manifests/latest", _Headers}, _HttpOpts, _Opts) ->
            {ok, {{http, 200, "OK"}, [], sample_manifest()}};
        (get, {"https://ghcr.io/v2/myorg/myapp/blobs/sha256:abc123", _Headers}, _HttpOpts, _Opts) ->
            {ok, {{http, 200, "OK"}, [], sample_config()}}
    end),

    Result = ocibuild_registry:pull_manifest(
        ~"ghcr.io",
        ~"myorg/myapp",
        ~"latest",
        #{token => ~"test-token"}
    ),

    ?assertMatch({ok, _, _}, Result),
    {ok, Manifest, Config} = Result,
    ?assertEqual(2, maps:get(~"schemaVersion", Manifest)),
    ?assertEqual(~"amd64", maps:get(~"architecture", Config)).

%%%===================================================================
%%% Pull blob tests
%%%===================================================================

pull_blob_test() ->
    BlobData = <<"binary blob data here">>,

    meck:expect(httpc, request, fun(
        get, {"https://ghcr.io/v2/myorg/myapp/blobs/sha256:blobdigest", _Headers}, _HttpOpts, _Opts
    ) ->
        {ok, {{http, 200, "OK"}, [], BlobData}}
    end),

    Result = ocibuild_registry:pull_blob(
        ~"ghcr.io",
        ~"myorg/myapp",
        ~"sha256:blobdigest",
        #{token => ~"test-token"}
    ),

    ?assertEqual({ok, BlobData}, Result).

%%%===================================================================
%%% Check blob exists tests
%%%===================================================================

check_blob_exists_true_test() ->
    meck:expect(httpc, request, fun(
        head, {"https://ghcr.io/v2/myorg/myapp/blobs/sha256:exists", _Headers}, _HttpOpts, _Opts
    ) ->
        {ok, {{http, 200, "OK"}, [], <<>>}}
    end),

    Result = ocibuild_registry:check_blob_exists(
        ~"ghcr.io",
        ~"myorg/myapp",
        ~"sha256:exists",
        #{token => ~"test-token"}
    ),

    ?assertEqual(true, Result).

check_blob_exists_false_test() ->
    meck:expect(httpc, request, fun(
        head, {"https://ghcr.io/v2/myorg/myapp/blobs/sha256:notfound", _Headers}, _HttpOpts, _Opts
    ) ->
        {ok, {{http, 404, "Not Found"}, [], <<>>}}
    end),

    Result = ocibuild_registry:check_blob_exists(
        ~"ghcr.io",
        ~"myorg/myapp",
        ~"sha256:notfound",
        #{token => ~"test-token"}
    ),

    ?assertEqual(false, Result).

%%%===================================================================
%%% HTTP handling tests
%%%===================================================================

http_error_test() ->
    meck:expect(httpc, request, fun(
        get, {"https://ghcr.io/v2/myorg/myapp/blobs/sha256:error", _Headers}, _HttpOpts, _Opts
    ) ->
        {ok, {{http, 500, "Internal Server Error"}, [], <<>>}}
    end),

    Result = ocibuild_registry:pull_blob(
        ~"ghcr.io",
        ~"myorg/myapp",
        ~"sha256:error",
        #{token => ~"test-token"}
    ),

    ?assertMatch({error, {http_error, 500, _}}, Result).

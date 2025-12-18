%%%-------------------------------------------------------------------
-module(ocibuild_registry_tests).
-moduledoc "Tests for OCI registry client with mocked HTTP".

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test fixtures
%%%===================================================================

%% Sample config JSON
sample_config() ->
    ocibuild_json:encode(#{
        ~"architecture" => ~"amd64",
        ~"os" => ~"linux",
        ~"config" => #{},
        ~"rootfs" => #{~"type" => ~"layers", ~"diff_ids" => []}
    }).

%% Calculate actual digest for sample config
sample_config_digest() ->
    ocibuild_digest:sha256(sample_config()).

%% Sample manifest JSON - uses actual digest of sample config
sample_manifest() ->
    ConfigDigest = sample_config_digest(),
    ConfigSize = byte_size(sample_config()),
    ocibuild_json:encode(#{
        ~"schemaVersion" => 2,
        ~"mediaType" => ~"application/vnd.oci.image.manifest.v1+json",
        ~"config" => #{
            ~"mediaType" => ~"application/vnd.oci.image.config.v1+json",
            ~"digest" => ConfigDigest,
            ~"size" => ConfigSize
        },
        ~"layers" => []
    }).

%%%===================================================================
%%% Setup / Teardown
%%%===================================================================

setup() ->
    %% Start required applications
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    %% Ensure any existing mock is unloaded first
    safe_unload(ocibuild_registry),
    %% Mock ocibuild_registry with passthrough - we'll mock http_get/http_head
    meck:new(ocibuild_registry, [unstick, passthrough]),
    ok.

%% Safely unload a mock module
safe_unload(Module) ->
    case is_mocked(Module) of
        true -> meck:unload(Module);
        false -> ok
    end.

%% Check if a module is currently mocked
is_mocked(Module) ->
    try meck:validate(Module) of
        _ -> true
    catch
        error:{not_mocked, _} -> false
    end.

cleanup(_) ->
    meck:unload(ocibuild_registry).

%%%===================================================================
%%% Test generators
%%%===================================================================

registry_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        {"pull manifest from generic registry", fun pull_manifest_generic_test/0},
        {"pull blob", fun pull_blob_test/0},
        {"pull blob rejects tampered content", fun pull_blob_digest_mismatch_test/0},
        {"check blob exists returns true", fun check_blob_exists_true_test/0},
        {"check blob exists returns false", fun check_blob_exists_false_test/0},
        {"http error handling", fun http_error_test/0}
    ]}.

%%%===================================================================
%%% Pull manifest tests
%%%===================================================================

pull_manifest_generic_test() ->
    %% Mock http_get to return manifest and config responses
    ConfigDigest = sample_config_digest(),
    ConfigUrl = "https://registry.example.io/v2/myorg/myapp/blobs/" ++ binary_to_list(ConfigDigest),
    meck:expect(ocibuild_registry, http_get, fun
        ("https://registry.example.io/v2/myorg/myapp/manifests/latest", _Headers) ->
            {ok, sample_manifest()};
        (Url, _Headers) when Url =:= ConfigUrl ->
            {ok, sample_config()}
    end),

    Result = ocibuild_registry:pull_manifest(
        ~"registry.example.io",
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
    BlobDigest = ocibuild_digest:sha256(BlobData),
    BlobUrl = "https://registry.example.io/v2/myorg/myapp/blobs/" ++ binary_to_list(BlobDigest),

    meck:expect(ocibuild_registry, http_get, fun(Url, _Headers) when Url =:= BlobUrl ->
        {ok, BlobData}
    end),

    Result = ocibuild_registry:pull_blob(
        ~"registry.example.io",
        ~"myorg/myapp",
        BlobDigest,
        #{token => ~"test-token"}
    ),

    ?assertEqual({ok, BlobData}, Result).

%% Test that tampered content is rejected (security fix for digest verification)
pull_blob_digest_mismatch_test() ->
    OriginalData = <<"original content">>,
    TamperedData = <<"tampered content">>,
    OriginalDigest = ocibuild_digest:sha256(OriginalData),
    BlobUrl = "https://registry.example.io/v2/myorg/myapp/blobs/" ++ binary_to_list(OriginalDigest),

    meck:expect(ocibuild_registry, http_get, fun(Url, _Headers) when Url =:= BlobUrl ->
        %% MITM or registry compromise returns different content
        {ok, TamperedData}
    end),

    Result = ocibuild_registry:pull_blob(
        ~"registry.example.io",
        ~"myorg/myapp",
        OriginalDigest,
        #{token => ~"test-token"}
    ),

    %% Should detect the mismatch and reject
    ?assertMatch({error, {digest_mismatch, _}}, Result).

%%%===================================================================
%%% Check blob exists tests
%%%===================================================================

check_blob_exists_true_test() ->
    meck:expect(ocibuild_registry, http_head, fun(
        "https://registry.example.io/v2/myorg/myapp/blobs/sha256:exists", _Headers
    ) ->
        {ok, []}
    end),

    Result = ocibuild_registry:check_blob_exists(
        ~"registry.example.io",
        ~"myorg/myapp",
        ~"sha256:exists",
        #{token => ~"test-token"}
    ),

    ?assertEqual(true, Result).

check_blob_exists_false_test() ->
    meck:expect(ocibuild_registry, http_head, fun(
        "https://registry.example.io/v2/myorg/myapp/blobs/sha256:notfound", _Headers
    ) ->
        {error, {http_error, 404, "Not Found"}}
    end),

    Result = ocibuild_registry:check_blob_exists(
        ~"registry.example.io",
        ~"myorg/myapp",
        ~"sha256:notfound",
        #{token => ~"test-token"}
    ),

    ?assertEqual(false, Result).

%%%===================================================================
%%% HTTP handling tests
%%%===================================================================

http_error_test() ->
    meck:expect(ocibuild_registry, http_get, fun(
        "https://registry.example.io/v2/myorg/myapp/blobs/sha256:error", _Headers
    ) ->
        {error, {http_error, 500, "Internal Server Error"}}
    end),

    Result = ocibuild_registry:pull_blob(
        ~"registry.example.io",
        ~"myorg/myapp",
        ~"sha256:error",
        #{token => ~"test-token"}
    ),

    ?assertMatch({error, {http_error, 500, _}}, Result).

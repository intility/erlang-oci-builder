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
        {"http error handling", fun http_error_test/0},
        %% Chunked upload tests
        {"small blob uses monolithic upload", fun small_blob_monolithic_test/0},
        {"large blob uses chunked upload", fun large_blob_chunked_test/0},
        {"multi-chunk blob has correct Content-Range", fun multi_chunk_content_range_test/0},
        {"progress callback receives updates", fun progress_callback_test/0},
        {"session start failure propagates error", fun session_start_error_test/0},
        {"chunk upload failure propagates error", fun chunk_upload_error_test/0},
        %% tag_from_digest tests
        {"tag_from_digest success", fun tag_from_digest_success_test/0},
        {"tag_from_digest with index manifest", fun tag_from_digest_index_manifest_test/0},
        {"tag_from_digest fetch error", fun tag_from_digest_fetch_error_test/0},
        {"tag_from_digest push error", fun tag_from_digest_push_error_test/0}
    ]}.

%% Chunked upload helper function tests (no mocking needed)
chunked_helper_test_() ->
    [
        {"format_content_range formats correctly", fun format_content_range_test/0},
        {"parse_range_header parses valid input", fun parse_range_header_valid_test/0},
        {"parse_range_header rejects invalid input", fun parse_range_header_invalid_test/0}
    ].

%% Note: http_get_with_content_type is tested indirectly through tag_from_digest tests
%% Direct testing of http_get_with_content_type would require mocking httpc,
%% which is difficult due to it being an OTP gen_server-based module.

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
    BlobData = ~"binary blob data here",
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
    OriginalData = ~"original content",
    TamperedData = ~"tampered content",
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

%%%===================================================================
%%% Chunked upload helper tests
%%%===================================================================

format_content_range_test() ->
    ?assertEqual("0-1048575", ocibuild_registry:format_content_range(0, 1048575)),
    ?assertEqual("1048576-2097151", ocibuild_registry:format_content_range(1048576, 2097151)),
    ?assertEqual("0-0", ocibuild_registry:format_content_range(0, 0)).

parse_range_header_valid_test() ->
    ?assertEqual({ok, 1048576}, ocibuild_registry:parse_range_header("0-1048575")),
    ?assertEqual({ok, 5242880}, ocibuild_registry:parse_range_header("0-5242879")),
    ?assertEqual({ok, 1}, ocibuild_registry:parse_range_header("0-0")).

parse_range_header_invalid_test() ->
    ?assertEqual(error, ocibuild_registry:parse_range_header("invalid")),
    ?assertEqual(error, ocibuild_registry:parse_range_header("")),
    ?assertEqual(error, ocibuild_registry:parse_range_header("abc-def")).

%%%===================================================================
%%% Chunked upload tests
%%%===================================================================

%% Create test data of specified size
make_test_data(Size) ->
    binary:copy(<<0>>, Size).

%% 1MB test data (small, uses monolithic)
small_blob() ->
    make_test_data(1 * 1024 * 1024).

%% 8MB test data (larger than 5MB default chunk, uses chunked)
large_blob() ->
    make_test_data(8 * 1024 * 1024).

%% 12MB test data (needs 2 PATCH + 1 PUT)
multi_chunk_blob() ->
    make_test_data(12 * 1024 * 1024).

small_blob_monolithic_test() ->
    %% Small blob (1MB) with default 5MB chunk size should use monolithic
    Data = small_blob(),
    Digest = ocibuild_digest:sha256(Data),

    %% Mock http_head to say blob doesn't exist
    meck:expect(ocibuild_registry, http_head, fun(_Url, _Headers) ->
        {error, {http_error, 404, "Not Found"}}
    end),

    %% Mock http_post for session start
    meck:expect(ocibuild_registry, http_post, fun(_Url, _Headers, <<>>) ->
        {ok, <<>>, [{"location", "/v2/test/blobs/uploads/uuid123"}]}
    end),

    %% Mock http_put for monolithic upload (http_put/4 with timeout)
    meck:expect(ocibuild_registry, http_put, 4, fun(_Url, _Headers, Body, _Timeout) ->
        ?assertEqual(byte_size(Data), byte_size(Body)),
        {ok, <<>>}
    end),

    %% Should NOT call http_patch for small blobs
    meck:expect(ocibuild_registry, http_patch, fun(_Url, _Headers, _Body, _Timeout) ->
        throw(unexpected_patch_call)
    end),

    Result = ocibuild_registry:push_blob(
        "https://registry.example.io",
        ~"test/repo",
        Digest,
        Data,
        ~"test-token",
        #{}
    ),
    %% If http_patch was called, the mock would throw and fail the test
    ?assertEqual(ok, Result).

large_blob_chunked_test() ->
    %% Large blob (8MB) with default 5MB chunk size should use chunked
    Data = large_blob(),
    Digest = ocibuild_digest:sha256(Data),
    ChunkSize = 5 * 1024 * 1024,

    %% Mock http_head to say blob doesn't exist
    meck:expect(ocibuild_registry, http_head, fun(_Url, _Headers) ->
        {error, {http_error, 404, "Not Found"}}
    end),

    %% Mock http_post for session start
    meck:expect(ocibuild_registry, http_post, fun(_Url, _Headers, <<>>) ->
        {ok, <<>>, [{"location", "/v2/test/blobs/uploads/uuid123"}]}
    end),

    %% Track PATCH calls
    PatchRef = make_ref(),
    put(PatchRef, 0),

    %% Mock http_patch for chunk uploads
    meck:expect(ocibuild_registry, http_patch, fun(_Url, _Headers, Body, _Timeout) ->
        Count = get(PatchRef),
        put(PatchRef, Count + 1),
        %% First chunk should be 5MB
        ?assertEqual(ChunkSize, byte_size(Body)),
        RangeEnd = (Count + 1) * ChunkSize - 1,
        {ok, 202, [{"range", io_lib:format("0-~B", [RangeEnd])}]}
    end),

    %% Mock http_put for final chunk (3MB remaining)
    %% complete_upload calls ?MODULE:http_put/4 with adaptive timeout
    meck:expect(ocibuild_registry, http_put, 4, fun(_Url, _Headers, Body, _Timeout) ->
        ExpectedFinalSize = 8 * 1024 * 1024 - 5 * 1024 * 1024,
        ?assertEqual(ExpectedFinalSize, byte_size(Body)),
        {ok, <<>>}
    end),

    Result = ocibuild_registry:push_blob(
        "https://registry.example.io",
        ~"test/repo",
        Digest,
        Data,
        ~"test-token",
        #{}
    ),
    ?assertEqual(ok, Result),

    %% Verify 1 PATCH call (5MB chunk) + 1 PUT (3MB final)
    ?assertEqual(1, get(PatchRef)).

multi_chunk_content_range_test() ->
    %% 12MB blob should have 2 PATCH calls with correct Content-Range headers
    Data = multi_chunk_blob(),
    Digest = ocibuild_digest:sha256(Data),
    ChunkSize = 5 * 1024 * 1024,

    meck:expect(ocibuild_registry, http_head, fun(_Url, _Headers) ->
        {error, {http_error, 404, "Not Found"}}
    end),

    meck:expect(ocibuild_registry, http_post, fun(_Url, _Headers, <<>>) ->
        {ok, <<>>, [{"location", "/v2/test/blobs/uploads/uuid123"}]}
    end),

    %% Track Content-Range headers
    RangeRef = make_ref(),
    put(RangeRef, []),

    meck:expect(ocibuild_registry, http_patch, fun(_Url, Headers, _Body, _Timeout) ->
        ContentRange = proplists:get_value("Content-Range", Headers),
        Ranges = get(RangeRef),
        put(RangeRef, Ranges ++ [ContentRange]),
        %% Return accumulated range
        CurrentEnd = length(Ranges ++ [ContentRange]) * ChunkSize - 1,
        {ok, 202, [{"range", io_lib:format("0-~B", [CurrentEnd])}]}
    end),

    meck:expect(ocibuild_registry, http_put, 4, fun(_Url, _Headers, _Body, _Timeout) ->
        {ok, <<>>}
    end),

    Result = ocibuild_registry:push_blob(
        "https://registry.example.io",
        ~"test/repo",
        Digest,
        Data,
        ~"test-token",
        #{}
    ),
    ?assertEqual(ok, Result),

    %% Verify Content-Range headers
    Ranges = get(RangeRef),
    ?assertEqual(2, length(Ranges)),
    ?assertEqual("0-5242879", lists:nth(1, Ranges)),
    ?assertEqual("5242880-10485759", lists:nth(2, Ranges)).

progress_callback_test() ->
    Data = large_blob(),
    Digest = ocibuild_digest:sha256(Data),
    Self = self(),

    meck:expect(ocibuild_registry, http_head, fun(_Url, _Headers) ->
        {error, {http_error, 404, "Not Found"}}
    end),

    meck:expect(ocibuild_registry, http_post, fun(_Url, _Headers, <<>>) ->
        {ok, <<>>, [{"location", "/v2/test/blobs/uploads/uuid123"}]}
    end),

    meck:expect(ocibuild_registry, http_patch, fun(_Url, _Headers, _Body, _Timeout) ->
        {ok, 202, [{"range", "0-5242879"}]}
    end),

    meck:expect(ocibuild_registry, http_put, 4, fun(_Url, _Headers, _Body, _Timeout) ->
        {ok, <<>>}
    end),

    %% Progress callback collects progress updates
    ProgressFn = fun(Info) ->
        Self ! {progress, Info}
    end,

    Result = ocibuild_registry:push_blob(
        "https://registry.example.io",
        ~"test/repo",
        Digest,
        Data,
        ~"test-token",
        #{progress => ProgressFn, layer_index => 0, total_layers => 1}
    ),
    ?assertEqual(ok, Result),

    %% Collect progress messages
    Progress = collect_progress([]),
    %% Expect at least 2 updates (start and final); extra intermediate updates are ok
    ?assert(length(Progress) >= 2),

    %% Verify progress structure
    [First | _] = Progress,
    ?assertEqual(uploading, maps:get(phase, First)),
    ?assert(maps:is_key(bytes_sent, First)),
    ?assert(maps:is_key(total_bytes, First)).

collect_progress(Acc) ->
    receive
        {progress, Info} -> collect_progress([Info | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.

session_start_error_test() ->
    Data = large_blob(),
    Digest = ocibuild_digest:sha256(Data),

    meck:expect(ocibuild_registry, http_head, fun(_Url, _Headers) ->
        {error, {http_error, 404, "Not Found"}}
    end),

    %% Session start fails
    meck:expect(ocibuild_registry, http_post, fun(_Url, _Headers, <<>>) ->
        {error, {http_error, 500, "Internal Server Error"}}
    end),

    Result = ocibuild_registry:push_blob(
        "https://registry.example.io",
        ~"test/repo",
        Digest,
        Data,
        ~"test-token",
        #{}
    ),
    ?assertMatch({error, {http_error, 500, _}}, Result).

chunk_upload_error_test() ->
    Data = large_blob(),
    Digest = ocibuild_digest:sha256(Data),

    meck:expect(ocibuild_registry, http_head, fun(_Url, _Headers) ->
        {error, {http_error, 404, "Not Found"}}
    end),

    meck:expect(ocibuild_registry, http_post, fun(_Url, _Headers, <<>>) ->
        {ok, <<>>, [{"location", "/v2/test/blobs/uploads/uuid123"}]}
    end),

    %% Chunk upload fails with 400 (non-retriable error to avoid retry delays)
    meck:expect(ocibuild_registry, http_patch, fun(_Url, _Headers, _Body, _Timeout) ->
        {error, {http_error, 400, "Bad Request"}}
    end),

    Result = ocibuild_registry:push_blob(
        "https://registry.example.io",
        ~"test/repo",
        Digest,
        Data,
        ~"test-token",
        #{}
    ),
    ?assertMatch({error, {http_error, 400, _}}, Result).

%%%===================================================================
%%% tag_from_digest tests
%%%===================================================================

tag_from_digest_success_test() ->
    %% Test successful tagging of manifest by digest to new tag
    ManifestData = sample_manifest(),
    ManifestDigest = ocibuild_digest:sha256(ManifestData),
    ContentType = "application/vnd.oci.image.manifest.v1+json",

    %% Mock discover_auth to skip HTTP calls
    meck:expect(ocibuild_registry, discover_auth, fun(_Registry, _Repo, _Auth) ->
        {ok, ~"mocked-token"}
    end),

    %% Mock http_get_with_content_type to return manifest
    meck:expect(ocibuild_registry, http_get_with_content_type, fun(Url, _Headers) ->
        ?assert(string:find(Url, binary_to_list(ManifestDigest)) =/= nomatch),
        {ok, ManifestData, ContentType}
    end),

    %% Mock http_put to verify manifest is pushed with correct content type
    meck:expect(ocibuild_registry, http_put, fun(Url, Headers, Body) ->
        ?assert(string:find(Url, "newtag") =/= nomatch),
        ?assertEqual(ContentType, proplists:get_value("Content-Type", Headers)),
        ?assertEqual(ManifestData, Body),
        {ok, <<>>}
    end),

    Result = ocibuild_registry:tag_from_digest(
        ~"registry.example.io",
        ~"myorg/myapp",
        ManifestDigest,
        ~"newtag",
        #{token => ~"test-token"}
    ),

    ?assertEqual({ok, ManifestDigest}, Result).

tag_from_digest_index_manifest_test() ->
    %% Test tagging with OCI index manifest (multi-platform)
    IndexData = ocibuild_json:encode(#{
        ~"schemaVersion" => 2,
        ~"mediaType" => ~"application/vnd.oci.image.index.v1+json",
        ~"manifests" => []
    }),
    IndexDigest = ocibuild_digest:sha256(IndexData),
    ContentType = "application/vnd.oci.image.index.v1+json",

    %% Mock discover_auth to skip HTTP calls
    meck:expect(ocibuild_registry, discover_auth, fun(_Registry, _Repo, _Auth) ->
        {ok, ~"mocked-token"}
    end),

    meck:expect(ocibuild_registry, http_get_with_content_type, fun(_Url, _Headers) ->
        {ok, IndexData, ContentType}
    end),

    meck:expect(ocibuild_registry, http_put, fun(_Url, Headers, _Body) ->
        %% Verify content type is preserved for index
        ?assertEqual(ContentType, proplists:get_value("Content-Type", Headers)),
        {ok, <<>>}
    end),

    Result = ocibuild_registry:tag_from_digest(
        ~"registry.example.io",
        ~"myorg/myapp",
        IndexDigest,
        ~"latest",
        #{token => ~"test-token"}
    ),

    ?assertEqual({ok, IndexDigest}, Result).

tag_from_digest_fetch_error_test() ->
    %% Test error handling when manifest fetch fails

    %% Mock discover_auth to skip HTTP calls
    meck:expect(ocibuild_registry, discover_auth, fun(_Registry, _Repo, _Auth) ->
        {ok, ~"mocked-token"}
    end),

    meck:expect(ocibuild_registry, http_get_with_content_type, fun(_Url, _Headers) ->
        {error, {http_error, 404, "Not Found", <<>>}}
    end),

    Result = ocibuild_registry:tag_from_digest(
        ~"registry.example.io",
        ~"myorg/myapp",
        ~"sha256:nonexistent",
        ~"newtag",
        #{token => ~"test-token"}
    ),

    ?assertMatch({error, {http_error, 404, _, _}}, Result).

tag_from_digest_push_error_test() ->
    %% Test error handling when manifest push fails
    ManifestData = sample_manifest(),
    ManifestDigest = ocibuild_digest:sha256(ManifestData),

    %% Mock discover_auth to skip HTTP calls
    meck:expect(ocibuild_registry, discover_auth, fun(_Registry, _Repo, _Auth) ->
        {ok, ~"mocked-token"}
    end),

    meck:expect(ocibuild_registry, http_get_with_content_type, fun(_Url, _Headers) ->
        {ok, ManifestData, "application/vnd.oci.image.manifest.v1+json"}
    end),

    meck:expect(ocibuild_registry, http_put, fun(_Url, _Headers, _Body) ->
        {error, {http_error, 403, "Forbidden"}}
    end),

    Result = ocibuild_registry:tag_from_digest(
        ~"registry.example.io",
        ~"myorg/myapp",
        ManifestDigest,
        ~"newtag",
        #{token => ~"test-token"}
    ),

    ?assertMatch({error, {http_error, 403, _}}, Result).


%%%===================================================================
%%% Chunked upload tests (continued)
%%%===================================================================

%% Test that 416 error falls back to monolithic upload (for registries like GHCR)
chunked_upload_416_fallback_test() ->
    Data = large_blob(),
    Digest = ocibuild_digest:sha256(Data),

    %% Track call sequence
    CallRef = make_ref(),
    put(CallRef, []),

    meck:expect(ocibuild_registry, http_head, fun(_Url, _Headers) ->
        %% Blob doesn't exist, needs upload
        {error, {http_error, 404, "Not Found"}}
    end),

    %% POST creates upload session (called twice - once for chunked, once for monolithic fallback)
    meck:expect(ocibuild_registry, http_post, fun(_Url, _Headers, <<>>) ->
        Calls = get(CallRef),
        put(CallRef, [post | Calls]),
        {ok, <<>>, [{"location", "/v2/test/blobs/uploads/uuid-" ++ integer_to_list(length(Calls))}]}
    end),

    %% PATCH fails with 416 (registry doesn't support chunked uploads)
    meck:expect(ocibuild_registry, http_patch, fun(_Url, _Headers, _Body, _Timeout) ->
        Calls = get(CallRef),
        put(CallRef, [patch | Calls]),
        {error, {http_error, 416, "Unexpected status from PATCH"}}
    end),

    %% PUT succeeds (for monolithic fallback) - note: http_put/4 is used with timeout
    meck:expect(ocibuild_registry, http_put, 4, fun(Url, _Headers, Body, _Timeout) ->
        Calls = get(CallRef),
        put(CallRef, [put | Calls]),
        %% Verify it's the full blob in monolithic upload
        ?assertEqual(byte_size(Data), byte_size(Body)),
        %% Verify the digest is in the URL
        ?assert(string:find(Url, binary_to_list(Digest)) =/= nomatch),
        {ok, <<>>}
    end),

    Result = ocibuild_registry:push_blob(
        "https://registry.example.io",
        ~"test/repo",
        Digest,
        Data,
        ~"test-token",
        #{}
    ),

    %% Verify call sequence: POST (chunked) -> PATCH (fails 416) -> POST (monolithic) -> PUT (success)
    Calls = lists:reverse(get(CallRef)),
    ?assertEqual([post, patch, post, put], Calls),
    ?assertEqual(ok, Result).

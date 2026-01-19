%%%-------------------------------------------------------------------
-module(ocibuild_registry).
-moduledoc """
OCI Distribution registry client.

Implements the OCI Distribution Specification for pulling and pushing
container images to registries like Docker Hub, GHCR, etc.

See: https://github.com/opencontainers/distribution-spec
""".

-export([
    pull_manifest/3, pull_manifest/4, pull_manifest/5,
    pull_manifests_for_platforms/5,
    pull_blob/3, pull_blob/4, pull_blob/5,
    push/5, push/6,
    push_multi/6,
    push_blobs/5, push_blobs/6,
    push_blobs_multi/5, push_blobs_multi/6,
    push_referrer/7, push_referrer/8,
    push_signature/7, push_signature/8,
    digest_to_signature_tag/1,
    check_blob_exists/4,
    tag_from_digest/5,
    stop_httpc/0
]).

%% Retry utilities (shared with ocibuild_layout)
-export([is_retriable_error/1, with_retry/2]).

%% Platform detection (shared with ocibuild_release)
-export([get_target_platform/0]).

%% Internal HTTP functions - exported for ?MODULE: calls (enables mocking in tests)
-export([http_get/2, http_head/2, http_post/3, http_patch/4, http_put/3, http_put/4]).
-export([http_get_with_content_type/2]).

%% Internal functions - exported for ?MODULE: calls (enables mocking in tests)
-export([push_blob/5, push_blob/6, format_content_range/2, parse_range_header/1]).
-export([discover_auth/3]).

%% Security functions - exported for testing
-ifdef(TEST).
-export([sanitize_error_body/1, redact_sensitive/1]).
-endif.

%% Progress callback types
-type progress_phase() :: manifest | config | layer | uploading.
-type progress_info() :: #{
    phase := progress_phase(),
    layer_index => non_neg_integer(),
    total_layers => non_neg_integer(),
    bytes_received := non_neg_integer(),
    bytes_sent => non_neg_integer(),
    total_bytes := non_neg_integer() | unknown
}.
-type progress_callback() :: fun((progress_info()) -> ok).
-type pull_opts() :: #{
    progress => progress_callback(),
    auth => map(),
    size => non_neg_integer(),
    layer_index => non_neg_integer(),
    total_layers => non_neg_integer()
}.

%% Chunked upload types
-type upload_session() :: #{
    upload_url := string(),
    bytes_uploaded := non_neg_integer()
}.
-type push_opts() :: #{
    chunk_size => pos_integer(),
    progress => progress_callback(),
    layer_index => non_neg_integer(),
    total_layers => non_neg_integer()
}.

%% Operation scope for token exchange
%% - pull: Only read access needed (for pulling base images)
%% - push: Read/write access needed (for pushing images)
-type operation_scope() :: pull | push.

-export_type([progress_callback/0, progress_info/0, pull_opts/0, push_opts/0, upload_session/0, operation_scope/0]).

-define(DEFAULT_TIMEOUT, 30000).
%% Default httpc profile for fallback mode (non-worker context)
-define(DEFAULT_HTTPC_PROFILE, ocibuild).
-define(HTTPC_KEY, {?MODULE, httpc_pid}).
%% Maximum concurrent uploads (to avoid rate limiting)
-define(MAX_CONCURRENT_UPLOADS, 4).

%% State record for chunked upload loop
-record(upload_state, {
    session :: upload_session(),
    data :: binary(),
    digest :: binary(),
    offset :: non_neg_integer(),
    chunk_size :: pos_integer(),
    total_size :: non_neg_integer(),
    token :: binary(),
    progress_fn :: progress_callback() | undefined,
    layer_index :: non_neg_integer(),
    total_layers :: non_neg_integer()
}).

%% State record for streaming download
-record(stream_state, {
    request_id :: reference(),
    progress_fn :: progress_callback() | undefined,
    phase :: progress_phase(),
    total_bytes :: non_neg_integer() | unknown,
    acc :: binary(),
    url :: string(),
    headers :: [{string(), string()}],
    redirects_left :: non_neg_integer(),
    layer_index :: non_neg_integer(),
    total_layers :: non_neg_integer()
}).

%% Registry URL mappings
-define(REGISTRY_URLS, #{
    ~"docker.io" => "https://registry-1.docker.io",
    ~"ghcr.io" => "https://ghcr.io",
    ~"gcr.io" => "https://gcr.io",
    ~"quay.io" => "https://quay.io"
}).

%%%===================================================================
%%% API
%%%===================================================================

-doc "Pull a manifest and config from a registry.".
-spec pull_manifest(binary(), binary(), binary()) ->
    {ok, Manifest :: map(), Config :: map()} | {error, term()}.
pull_manifest(Registry, Repo, Ref) ->
    pull_manifest(Registry, Repo, Ref, #{}).

-doc "Pull a manifest and config from a registry with authentication.".
-spec pull_manifest(binary(), binary(), binary(), map()) ->
    {ok, Manifest :: map(), Config :: map()} | {error, term()}.
pull_manifest(Registry, Repo, Ref, Auth) ->
    pull_manifest(Registry, Repo, Ref, Auth, #{}).

-doc "Pull a manifest and config from a registry with options.".
-spec pull_manifest(binary(), binary(), binary(), map(), pull_opts()) ->
    {ok, Manifest :: map(), Config :: map()} | {error, term()}.
pull_manifest(Registry, Repo, Ref, Auth, Opts) ->
    BaseUrl = registry_url(Registry),
    NormalizedRepo = normalize_repo(Registry, Repo),
    ProgressFn = maps:get(progress, Opts, undefined),

    %% Get auth token if needed - pull only needs read access
    case get_auth_token(Registry, NormalizedRepo, Auth, pull) of
        {ok, Token} ->
            %% Fetch manifest (accept both single manifests and manifest lists)
            ManifestUrl =
                io_lib:format(
                    "~s/v2/~s/manifests/~s",
                    [BaseUrl, binary_to_list(NormalizedRepo), binary_to_list(Ref)]
                ),
            %% Combine Accept types into single header (httpc may not handle multiple Accept headers correctly)
            AcceptTypes = "application/vnd.oci.image.index.v1+json, "
                          "application/vnd.docker.distribution.manifest.list.v2+json, "
                          "application/vnd.oci.image.manifest.v1+json, "
                          "application/vnd.docker.distribution.manifest.v2+json",
            Headers =
                auth_headers(Token) ++
                    [{"Accept", AcceptTypes}],

            %% Report manifest phase
            maybe_report_progress(ProgressFn, #{
                phase => manifest,
                bytes_received => 0,
                total_bytes => unknown
            }),

            case ?MODULE:http_get(lists:flatten(ManifestUrl), Headers) of
                {ok, ManifestJson} ->
                    Manifest = ocibuild_json:decode(ManifestJson),
                    %% Check if this is a manifest list (multi-platform image)
                    case is_manifest_list(Manifest) of
                        true ->
                            %% Select platform-specific manifest and fetch it
                            %% If platform option is provided, use it; otherwise auto-detect
                            SelectedResult =
                                case maps:find(platform, Opts) of
                                    {ok, Platform} ->
                                        select_platform_manifest(Manifest, Platform);
                                    error ->
                                        select_platform_manifest(Manifest)
                                end,
                            case SelectedResult of
                                {ok, PlatformDigest} ->
                                    pull_manifest(Registry, Repo, PlatformDigest, Auth, Opts);
                                {error, _} = Err ->
                                    Err
                            end;
                        false ->
                            %% Single manifest - fetch config blob
                            ConfigDescriptor = maps:get(~"config", Manifest),
                            ConfigDigest = maps:get(~"digest", ConfigDescriptor),
                            ConfigSize = maps:get(~"size", ConfigDescriptor, unknown),

                            %% Report config phase
                            maybe_report_progress(ProgressFn, #{
                                phase => config,
                                bytes_received => 0,
                                total_bytes => ConfigSize
                            }),

                            case
                                pull_blob_internal(
                                    Registry,
                                    Repo,
                                    ConfigDigest,
                                    Auth,
                                    ProgressFn,
                                    config,
                                    ConfigSize,
                                    0,
                                    1
                                )
                            of
                                {ok, ConfigJson} ->
                                    Config = ocibuild_json:decode(ConfigJson),
                                    {ok, Manifest, Config};
                                {error, _} = Err ->
                                    Err
                            end
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

-doc """
Pull manifests and configs for multiple platforms.

Given a list of platforms, this function pulls the manifest and config
for each platform from the base image. Used for multi-platform builds.

Returns a list of `{Platform, Manifest, Config}` tuples, one for each requested platform.
""".
-spec pull_manifests_for_platforms(binary(), binary(), binary(), map(), [ocibuild:platform()]) ->
    {ok, [{ocibuild:platform(), map(), map()}]} | {error, term()}.
pull_manifests_for_platforms(Registry, Repo, Ref, Auth, Platforms) ->
    Results = lists:map(
        fun(Platform) ->
            Opts = #{platform => Platform},
            case pull_manifest(Registry, Repo, Ref, Auth, Opts) of
                {ok, Manifest, Config} ->
                    {ok, {Platform, Manifest, Config}};
                {error, _} = Err ->
                    Err
            end
        end,
        Platforms
    ),
    %% Check if any failed
    case
        lists:partition(
            fun
                ({ok, _}) -> true;
                ({error, _}) -> false
            end,
            Results
        )
    of
        {Successes, []} ->
            {ok, [R || {ok, R} <- Successes]};
        {_, [FirstError | _]} ->
            FirstError
    end.

-doc "Pull a blob from a registry.".
-spec pull_blob(binary(), binary(), binary()) -> {ok, binary()} | {error, term()}.
pull_blob(Registry, Repo, Digest) ->
    pull_blob(Registry, Repo, Digest, #{}).

-doc "Pull a blob from a registry with authentication.".
-spec pull_blob(binary(), binary(), binary(), map()) -> {ok, binary()} | {error, term()}.
pull_blob(Registry, Repo, Digest, Auth) ->
    pull_blob(Registry, Repo, Digest, Auth, #{}).

-doc "Pull a blob from a registry with options including progress callback.".
-spec pull_blob(binary(), binary(), binary(), map(), pull_opts()) ->
    {ok, binary()} | {error, term()}.
pull_blob(Registry, Repo, Digest, Auth, Opts) ->
    ProgressFn = maps:get(progress, Opts, undefined),
    TotalBytes = maps:get(size, Opts, unknown),
    LayerIndex = maps:get(layer_index, Opts, 0),
    TotalLayers = maps:get(total_layers, Opts, 1),
    pull_blob_internal(
        Registry, Repo, Digest, Auth, ProgressFn, layer, TotalBytes, LayerIndex, TotalLayers
    ).

%% Internal blob pull with progress support
-spec pull_blob_internal(
    binary(),
    binary(),
    binary(),
    map(),
    progress_callback() | undefined,
    progress_phase(),
    non_neg_integer() | unknown,
    non_neg_integer(),
    non_neg_integer()
) ->
    {ok, binary()} | {error, term()}.
pull_blob_internal(
    Registry, Repo, Digest, Auth, ProgressFn, Phase, TotalBytes, LayerIndex, TotalLayers
) ->
    BaseUrl = registry_url(Registry),
    NormalizedRepo = normalize_repo(Registry, Repo),

    %% Pull blob only needs read access
    case get_auth_token(Registry, NormalizedRepo, Auth, pull) of
        {ok, Token} ->
            Url = io_lib:format(
                "~s/v2/~s/blobs/~s",
                [BaseUrl, binary_to_list(NormalizedRepo), binary_to_list(Digest)]
            ),
            Headers = auth_headers(Token),
            ProgressOpts = #{layer_index => LayerIndex, total_layers => TotalLayers},
            case
                http_get_with_progress(
                    lists:flatten(Url), Headers, ProgressFn, Phase, TotalBytes, ProgressOpts
                )
            of
                {ok, Data} ->
                    %% Verify downloaded content matches expected digest
                    verify_digest(Data, Digest);
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% Verify that downloaded content matches expected digest
-spec verify_digest(binary(), binary()) -> {ok, binary()} | {error, term()}.
verify_digest(Data, ExpectedDigest) ->
    ActualDigest = ocibuild_digest:sha256(Data),
    case ActualDigest =:= ExpectedDigest of
        true ->
            {ok, Data};
        false ->
            {error, {digest_mismatch, #{expected => ExpectedDigest, actual => ActualDigest}}}
    end.

-doc "Check if a blob exists in the registry.".
-spec check_blob_exists(binary(), binary(), binary(), map()) -> boolean().
check_blob_exists(Registry, Repo, Digest, Auth) ->
    BaseUrl = registry_url(Registry),
    NormalizedRepo = normalize_repo(Registry, Repo),

    %% Check blob exists only needs read access
    case get_auth_token(Registry, NormalizedRepo, Auth, pull) of
        {ok, Token} ->
            Url = io_lib:format(
                "~s/v2/~s/blobs/~s",
                [BaseUrl, binary_to_list(NormalizedRepo), binary_to_list(Digest)]
            ),
            Headers = auth_headers(Token),
            case ?MODULE:http_head(lists:flatten(Url), Headers) of
                {ok, _} ->
                    true;
                {error, _} ->
                    false
            end;
        {error, _} ->
            false
    end.

-doc "Push an image to a registry. Returns the manifest digest on success.".
-spec push(ocibuild:image(), binary(), binary(), binary(), map()) ->
    {ok, Digest :: binary()} | {error, term()}.
push(Image, Registry, Repo, Tag, Auth) ->
    push(Image, Registry, Repo, Tag, Auth, #{}).

-doc "Push an image to a registry with options (supports chunked uploads). Returns the manifest digest on success.".
-spec push(ocibuild:image(), binary(), binary(), binary(), map(), push_opts()) ->
    {ok, Digest :: binary()} | {error, term()}.
push(Image, Registry, Repo, Tag, Auth, Opts) ->
    BaseUrl = registry_url(Registry),
    NormalizedRepo = normalize_repo(Registry, Repo),

    %% Push requires read+write access
    case get_auth_token(Registry, NormalizedRepo, Auth, push) of
        {ok, Token} ->
            %% Push layers
            case push_layers(Image, BaseUrl, NormalizedRepo, Token, Opts) of
                ok ->
                    %% Push config (no chunked upload needed - configs are small)
                    case push_config(Image, BaseUrl, NormalizedRepo, Token) of
                        {ok, ConfigDigest, ConfigSize} ->
                            %% Push manifest and return digest
                            push_manifest(Image, BaseUrl, NormalizedRepo, Tag, Token, #{
                                config_digest => ConfigDigest,
                                config_size => ConfigSize
                            });
                        {error, _} = Err ->
                            Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

-doc """
Push multiple platform-specific images as a single multi-platform image.

This function:
1. Pushes all layers, configs, and manifests for each platform
2. Creates an OCI image index referencing all platform manifests
3. Tags the index so the registry serves the correct platform to clients

Each image in the list must have a `platform` field set.

Returns the digest of the image index on success.
""".
-spec push_multi([ocibuild:image()], binary(), binary(), binary(), map(), push_opts()) ->
    {ok, Digest :: binary()} | {error, term()}.
push_multi([], _Registry, _Repo, _Tag, _Auth, _Opts) ->
    {error, no_images_to_push};
push_multi(Images, Registry, Repo, Tag, Auth, Opts) ->
    BaseUrl = registry_url(Registry),
    NormalizedRepo = normalize_repo(Registry, Repo),

    %% Push requires read+write access
    case get_auth_token(Registry, NormalizedRepo, Auth, push) of
        {ok, Token} ->
            %% Push each platform image and collect manifest info
            case push_platform_images(Images, BaseUrl, NormalizedRepo, Token, Opts, []) of
                {ok, ManifestDescriptors} ->
                    %% Create and push the image index, return digest
                    push_image_index(BaseUrl, NormalizedRepo, Tag, Token, ManifestDescriptors);
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% Push all platform images in parallel and collect their manifest descriptors
-spec push_platform_images([ocibuild:image()], string(), binary(), term(), push_opts(), list()) ->
    {ok, [{ocibuild:platform(), binary(), non_neg_integer()}]} | {error, term()}.
push_platform_images([], _BaseUrl, _Repo, _Token, _Opts, _Acc) ->
    {ok, []};
push_platform_images(Images, BaseUrl, Repo, Token, Opts, _Acc) ->
    %% Push all platform images in parallel with supervised HTTP workers
    PushFn = fun(Image) ->
        push_single_platform_image(Image, BaseUrl, Repo, Token, Opts)
    end,
    try
        Results = ocibuild_http:pmap(PushFn, Images, ?MAX_CONCURRENT_UPLOADS),
        {ok, Results}
    catch
        error:{platform_push_failed, Reason} ->
            {error, Reason}
    end.

%% Push a single platform image (layers, config, manifest) and return descriptor
-spec push_single_platform_image(ocibuild:image(), string(), binary(), term(), push_opts()) ->
    {ocibuild:platform(), binary(), non_neg_integer()}.
push_single_platform_image(Image, BaseUrl, Repo, Token, Opts) ->
    Platform = maps:get(platform, Image, #{os => ~"linux", architecture => ~"amd64"}),
    %% Push layers
    case push_layers(Image, BaseUrl, Repo, Token, Opts) of
        ok ->
            %% Push config
            case push_config(Image, BaseUrl, Repo, Token) of
                {ok, ConfigDigest, ConfigSize} ->
                    %% Push manifest (without tagging)
                    case
                        push_manifest_untagged(
                            Image, BaseUrl, Repo, Token, ConfigDigest, ConfigSize
                        )
                    of
                        {ok, ManifestDigest, ManifestSize} ->
                            {Platform, ManifestDigest, ManifestSize};
                        {error, Err} ->
                            error({platform_push_failed, {manifest_failed, Platform, Err}})
                    end;
                {error, Err} ->
                    error({platform_push_failed, {config_failed, Platform, Err}})
            end;
        {error, Err} ->
            error({platform_push_failed, {layers_failed, Platform, Err}})
    end.

%% Push a manifest without tagging (returns digest and size)
-spec push_manifest_untagged(
    ocibuild:image(), string(), binary(), term(), binary(), non_neg_integer()
) ->
    {ok, binary(), non_neg_integer()} | {error, term()}.
push_manifest_untagged(Image, BaseUrl, Repo, Token, ConfigDigest, ConfigSize) ->
    %% Get base image layers if present (these already exist in registry)
    BaseLayerDescriptors =
        case maps:get(base_manifest, Image, undefined) of
            undefined ->
                [];
            BaseManifest ->
                maps:get(~"layers", BaseManifest, [])
        end,

    %% Our layers are stored in reverse order, reverse for correct manifest order
    NewLayerDescriptors =
        [
            #{
                ~"mediaType" => MediaType,
                ~"digest" => Digest,
                ~"size" => Size
            }
         || #{
                media_type := MediaType,
                digest := Digest,
                size := Size
            } <-
                lists:reverse(maps:get(layers, Image, []))
        ],

    %% Combine: base layers first, then our new layers
    LayerDescriptors = BaseLayerDescriptors ++ NewLayerDescriptors,

    %% Get annotations from image
    Annotations = maps:get(annotations, Image, #{}),

    {ManifestJson, _} =
        ocibuild_manifest:build(
            #{
                ~"mediaType" =>
                    ~"application/vnd.oci.image.config.v1+json",
                ~"digest" => ConfigDigest,
                ~"size" => ConfigSize
            },
            LayerDescriptors,
            Annotations
        ),

    ManifestDigest = ocibuild_digest:sha256(ManifestJson),
    ManifestSize = byte_size(ManifestJson),

    %% Push manifest by digest (not by tag)
    Url = io_lib:format(
        "~s/v2/~s/manifests/~s",
        [BaseUrl, binary_to_list(Repo), binary_to_list(ManifestDigest)]
    ),
    Headers =
        auth_headers(Token) ++
            [{"Content-Type", "application/vnd.oci.image.manifest.v1+json"}],

    case ?MODULE:http_put(lists:flatten(Url), Headers, ManifestJson) of
        {ok, _} ->
            {ok, ManifestDigest, ManifestSize};
        {error, _} = Err ->
            Err
    end.

%% Push the image index and tag it, return the index digest
-spec push_image_index(
    string(),
    binary(),
    binary(),
    term(),
    [{ocibuild:platform(), binary(), non_neg_integer()}]
) -> {ok, Digest :: binary()} | {error, term()}.
push_image_index(BaseUrl, Repo, Tag, Token, ManifestDescriptors) ->
    Index = ocibuild_index:create(ManifestDescriptors),
    IndexJson = ocibuild_index:to_json(Index),
    IndexDigest = ocibuild_digest:sha256(IndexJson),

    %% Push index with tag
    Url = io_lib:format(
        "~s/v2/~s/manifests/~s",
        [BaseUrl, binary_to_list(Repo), binary_to_list(Tag)]
    ),
    Headers =
        auth_headers(Token) ++
            [{"Content-Type", "application/vnd.oci.image.index.v1+json"}],

    case ?MODULE:http_put(lists:flatten(Url), Headers, IndexJson) of
        {ok, _} ->
            {ok, IndexDigest};
        {error, _} = Err ->
            Err
    end.

%%%===================================================================
%%% Tag existing manifest with new tag
%%%===================================================================

-doc """
Tag an existing manifest with a new tag.

This function fetches a manifest by its digest from the registry and
pushes it to a new tag. This is efficient because it doesn't re-upload
blobs - it only creates a new tag reference to the same manifest.

This is used to push an image with multiple tags: the first tag does
a full push (blobs + manifest), and additional tags use this function
to just create new tag references.
""".
-spec tag_from_digest(binary(), binary(), binary(), binary(), map()) ->
    {ok, binary()} | {error, term()}.
tag_from_digest(Registry, Repo, Digest, Tag, Auth) ->
    BaseUrl = registry_url(Registry),
    NormalizedRepo = normalize_repo(Registry, Repo),

    %% Discover auth token with push scope (uses ?MODULE: for testability)
    Token = case ?MODULE:discover_auth(Registry, NormalizedRepo, Auth) of
        {ok, T} -> T;
        {error, _} -> none
    end,

    %% Fetch manifest by digest with multiple Accept types
    %% We accept both image manifest and index manifest
    FetchUrl = io_lib:format(
        "~s/v2/~s/manifests/~s",
        [BaseUrl, binary_to_list(NormalizedRepo), binary_to_list(Digest)]
    ),
    AcceptHeader = {"Accept", "application/vnd.oci.image.manifest.v1+json, "
                              "application/vnd.oci.image.index.v1+json, "
                              "application/vnd.docker.distribution.manifest.v2+json, "
                              "application/vnd.docker.distribution.manifest.list.v2+json"},
    FetchHeaders = auth_headers(Token) ++ [AcceptHeader],

    case ?MODULE:http_get_with_content_type(lists:flatten(FetchUrl), FetchHeaders) of
        {ok, ManifestData, ContentType} ->
            %% Push to new tag with same content type
            PushUrl = io_lib:format(
                "~s/v2/~s/manifests/~s",
                [BaseUrl, binary_to_list(NormalizedRepo), binary_to_list(Tag)]
            ),
            PushHeaders = auth_headers(Token) ++ [{"Content-Type", ContentType}],
            case ?MODULE:http_put(lists:flatten(PushUrl), PushHeaders, ManifestData) of
                {ok, _} ->
                    {ok, Digest};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% @private HTTP GET that also returns the Content-Type header
-spec http_get_with_content_type(string(), [{string(), string()}]) ->
    {ok, binary(), string()} | {error, term()}.
http_get_with_content_type(Url, Headers) ->
    Profile = get_httpc_profile(),
    Request = {Url, Headers},
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}, {ssl, ssl_opts()}],
    Opts = [{body_format, binary}],
    case httpc:request(get, Request, HttpOpts, Opts, Profile) of
        {ok, {{_, 200, _}, RespHeaders, Body}} ->
            ContentType = proplists:get_value("content-type", normalize_headers(RespHeaders),
                                              "application/vnd.oci.image.manifest.v1+json"),
            {ok, Body, ContentType};
        {ok, {{_, Code, Reason}, _, Body}} ->
            %% Sanitize error body to prevent credential leakage
            {error, {http_error, Code, Reason, sanitize_error_body(Body)}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%%%===================================================================
%%% Push pre-built blobs (for pushing existing tarballs)
%%%===================================================================

%% Type for push_blobs input
-type push_blobs_input() :: #{
    manifest := binary(),
    config := binary(),
    layers := [#{
        digest := binary(),
        size := non_neg_integer(),
        get_data := fun(() -> {ok, binary()} | {error, term()})
    }]
}.

-doc """
Push a pre-built image from raw blobs to a registry.

This function is used to push images loaded from tarballs without
rebuilding. It reuses all existing push infrastructure (authentication,
progress callbacks, chunked uploads, etc.).

Blobs contains:
- manifest: Raw manifest JSON
- config: Raw config JSON
- layers: List of layer info with lazy `get_data` functions

Returns the manifest digest on success.
""".
-spec push_blobs(binary(), binary(), binary(), push_blobs_input(), map()) ->
    {ok, Digest :: binary()} | {error, term()}.
push_blobs(Registry, Repo, Tag, Blobs, Auth) ->
    push_blobs(Registry, Repo, Tag, Blobs, Auth, #{}).

-spec push_blobs(binary(), binary(), binary(), push_blobs_input(), map(), push_opts()) ->
    {ok, Digest :: binary()} | {error, term()}.
push_blobs(Registry, Repo, Tag, Blobs, Auth, Opts) ->
    BaseUrl = registry_url(Registry),
    NormalizedRepo = normalize_repo(Registry, Repo),
    ConfigData = maps:get(config, Blobs),
    ConfigDigest = ocibuild_digest:sha256(ConfigData),
    ManifestData = maps:get(manifest, Blobs),

    maybe
        %% Push requires read+write access
        {ok, Token} ?= get_auth_token(Registry, NormalizedRepo, Auth, push),
        ok ?= push_blob_list(maps:get(layers, Blobs), BaseUrl, NormalizedRepo, Token, Opts),
        ok ?= push_blob(BaseUrl, NormalizedRepo, ConfigDigest, ConfigData, Token),
        push_manifest_raw(ManifestData, BaseUrl, NormalizedRepo, Tag, Token)
    end.

-doc """
Push multiple pre-built images as a multi-platform image.

Each image in the list should have platform information.
Creates an image index referencing all platform manifests.
""".
-spec push_blobs_multi(binary(), binary(), binary(), [push_blobs_input()], map()) ->
    {ok, Digest :: binary()} | {error, term()}.
push_blobs_multi(Registry, Repo, Tag, BlobsList, Auth) ->
    push_blobs_multi(Registry, Repo, Tag, BlobsList, Auth, #{}).

-spec push_blobs_multi(binary(), binary(), binary(), [push_blobs_input()], map(), push_opts()) ->
    {ok, Digest :: binary()} | {error, term()}.
push_blobs_multi(_Registry, _Repo, _Tag, [], _Auth, _Opts) ->
    {error, no_images_to_push};
push_blobs_multi(Registry, Repo, Tag, BlobsList, Auth, Opts) ->
    BaseUrl = registry_url(Registry),
    NormalizedRepo = normalize_repo(Registry, Repo),

    maybe
        %% Push requires read+write access
        {ok, Token} ?= get_auth_token(Registry, NormalizedRepo, Auth, push),
        {ok, ManifestDescriptors} ?= push_blobs_platforms(BlobsList, BaseUrl, NormalizedRepo, Token, Opts),
        push_image_index(BaseUrl, NormalizedRepo, Tag, Token, ManifestDescriptors)
    end.

%% Push all platform images and collect manifest descriptors
-spec push_blobs_platforms([push_blobs_input()], string(), binary(), term(), push_opts()) ->
    {ok, [{ocibuild:platform(), binary(), non_neg_integer()}]} | {error, term()}.
push_blobs_platforms(BlobsList, BaseUrl, Repo, Token, Opts) ->
    PushFn = fun(Blobs) ->
        push_single_blobs_platform(Blobs, BaseUrl, Repo, Token, Opts)
    end,
    try
        Results = ocibuild_http:pmap(PushFn, BlobsList, ?MAX_CONCURRENT_UPLOADS),
        {ok, Results}
    catch
        error:{platform_push_failed, Reason} ->
            {error, Reason}
    end.

%% Push a single platform's blobs and return descriptor
-spec push_single_blobs_platform(push_blobs_input(), string(), binary(), term(), push_opts()) ->
    {ocibuild:platform(), binary(), non_neg_integer()}.
push_single_blobs_platform(Blobs, BaseUrl, Repo, Token, Opts) ->
    ManifestData = maps:get(manifest, Blobs),
    ConfigData = maps:get(config, Blobs),
    ConfigDigest = ocibuild_digest:sha256(ConfigData),

    %% Extract platform from config
    Config = ocibuild_json:decode(ConfigData),
    Platform = #{
        os => maps:get(~"os", Config, ~"linux"),
        architecture => maps:get(~"architecture", Config, ~"amd64")
    },

    %% Push layers
    case push_blob_list(maps:get(layers, Blobs), BaseUrl, Repo, Token, Opts) of
        ok ->
            %% Push config
            case push_blob(BaseUrl, Repo, ConfigDigest, ConfigData, Token) of
                ok ->
                    %% Push manifest by digest
                    ManifestDigest = ocibuild_digest:sha256(ManifestData),
                    ManifestSize = byte_size(ManifestData),
                    Url = io_lib:format(
                        "~s/v2/~s/manifests/~s",
                        [BaseUrl, binary_to_list(Repo), binary_to_list(ManifestDigest)]
                    ),
                    Headers =
                        auth_headers(Token) ++
                            [{"Content-Type", "application/vnd.oci.image.manifest.v1+json"}],
                    case ?MODULE:http_put(lists:flatten(Url), Headers, ManifestData) of
                        {ok, _} ->
                            {Platform, ManifestDigest, ManifestSize};
                        {error, Err} ->
                            error({platform_push_failed, {manifest_failed, Platform, Err}})
                    end;
                {error, Err} ->
                    error({platform_push_failed, {config_failed, Platform, Err}})
            end;
        {error, Err} ->
            error({platform_push_failed, {layers_failed, Platform, Err}})
    end.

%% Push a list of layers with lazy loading
-spec push_blob_list(
    [#{digest := binary(), size := non_neg_integer(),
       get_data := fun(() -> {ok, binary()} | {error, term()})}],
    string(), binary(), term(), push_opts()
) -> ok | {error, term()}.
push_blob_list([], _BaseUrl, _Repo, _Token, _Opts) ->
    ok;
push_blob_list(Layers, BaseUrl, Repo, Token, Opts) ->
    TotalLayers = length(Layers),
    IndexedLayers = lists:zip(lists:seq(1, TotalLayers), Layers),

    PushFn = fun({Index, #{digest := Digest, size := _Size, get_data := GetData}}) ->
        %% Check if blob already exists
        case blob_exists_with_token(BaseUrl, Repo, Digest, Token) of
            true ->
                ok;
            false ->
                %% Load data lazily and push
                case GetData() of
                    {ok, Data} ->
                        LayerOpts = Opts#{
                            layer_index => Index,
                            total_layers => TotalLayers
                        },
                        case push_blob(BaseUrl, Repo, Digest, Data, Token, LayerOpts) of
                            ok -> ok;
                            {error, _} = Err -> error(Err)
                        end;
                    {error, _} = Err ->
                        error(Err)
                end
        end
    end,

    try
        %% Push layers in parallel
        _ = ocibuild_http:pmap(PushFn, IndexedLayers, ?MAX_CONCURRENT_UPLOADS),
        ok
    catch
        error:{error, Reason} ->
            {error, Reason}
    end.

%% Push raw manifest JSON to registry
-spec push_manifest_raw(binary(), string(), binary(), binary(), term()) ->
    {ok, Digest :: binary()} | {error, term()}.
push_manifest_raw(ManifestData, BaseUrl, Repo, Tag, Token) ->
    ManifestDigest = ocibuild_digest:sha256(ManifestData),
    Url = io_lib:format(
        "~s/v2/~s/manifests/~s",
        [BaseUrl, binary_to_list(Repo), binary_to_list(Tag)]
    ),
    Headers =
        auth_headers(Token) ++
            [{"Content-Type", "application/vnd.oci.image.manifest.v1+json"}],

    case ?MODULE:http_put(lists:flatten(Url), Headers, ManifestData) of
        {ok, _} ->
            {ok, ManifestDigest};
        {error, _} = Err ->
            Err
    end.

%% Check if blob exists (internal - uses pre-authenticated token)
-spec blob_exists_with_token(string(), binary(), binary(), term()) -> boolean().
blob_exists_with_token(BaseUrl, Repo, Digest, Token) ->
    Url = io_lib:format(
        "~s/v2/~s/blobs/~s",
        [BaseUrl, binary_to_list(Repo), binary_to_list(Digest)]
    ),
    Headers = auth_headers(Token),
    case ?MODULE:http_head(lists:flatten(Url), Headers) of
        {ok, _} -> true;
        {error, _} -> false
    end.

%%%===================================================================
%%% OCI Referrer Push (for SBOM and other artifacts)
%%%===================================================================

-doc """
Push an artifact as a referrer to an image manifest.

This implements the OCI Referrers API, attaching artifacts (like SBOMs)
to an existing image manifest. The artifact manifest includes a `subject`
field that references the target image.

ArtifactData is the raw artifact content (e.g., SPDX JSON).
ArtifactType is the media type (e.g., "application/spdx+json").
SubjectDigest is the digest of the image manifest this artifact refers to.
SubjectSize is the size of the subject manifest.

Returns ok on success, or {error, {referrer_not_supported, _}} if the
registry doesn't support the referrers API.
""".
-spec push_referrer(
    ArtifactData :: binary(),
    ArtifactType :: binary(),
    Registry :: binary(),
    Repo :: binary(),
    SubjectDigest :: binary(),
    SubjectSize :: non_neg_integer(),
    Auth :: map()
) -> ok | {error, term()}.
push_referrer(ArtifactData, ArtifactType, Registry, Repo, SubjectDigest, SubjectSize, Auth) ->
    push_referrer(
        ArtifactData, ArtifactType, Registry, Repo, SubjectDigest, SubjectSize, Auth, #{}
    ).

-doc "Push a referrer artifact with options.".
-spec push_referrer(
    ArtifactData :: binary(),
    ArtifactType :: binary(),
    Registry :: binary(),
    Repo :: binary(),
    SubjectDigest :: binary(),
    SubjectSize :: non_neg_integer(),
    Auth :: map(),
    Opts :: push_opts()
) -> ok | {error, term()}.
push_referrer(ArtifactData, _ArtifactType, Registry, Repo, SubjectDigest, SubjectSize, Auth, _Opts) ->
    BaseUrl = registry_url(Registry),
    NormalizedRepo = normalize_repo(Registry, Repo),

    %% Push requires read+write access
    case get_auth_token(Registry, NormalizedRepo, Auth, push) of
        {ok, Token} ->
            %% Push the artifact blob
            ArtifactDigest = ocibuild_digest:sha256(ArtifactData),
            ArtifactSize = byte_size(ArtifactData),

            case push_blob(BaseUrl, NormalizedRepo, ArtifactDigest, ArtifactData, Token) of
                ok ->
                    %% Build and push the referrer manifest
                    push_referrer_manifest(
                        BaseUrl,
                        NormalizedRepo,
                        Token,
                        ArtifactDigest,
                        ArtifactSize,
                        SubjectDigest,
                        SubjectSize
                    );
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% @private Build and push a referrer manifest
%% Uses ocibuild_sbom:build_referrer_manifest/4 for manifest generation
-spec push_referrer_manifest(
    BaseUrl :: string(),
    Repo :: binary(),
    Token :: term(),
    ArtifactDigest :: binary(),
    ArtifactSize :: non_neg_integer(),
    SubjectDigest :: binary(),
    SubjectSize :: non_neg_integer()
) -> ok | {error, term()}.
push_referrer_manifest(
    BaseUrl, Repo, Token, ArtifactDigest, ArtifactSize, SubjectDigest, SubjectSize
) ->
    %% Push the empty config blob (required by OCI spec for artifact manifests)
    EmptyConfigBlob = ~"{}",
    EmptyConfigDigest = ocibuild_digest:sha256(EmptyConfigBlob),

    case push_blob(BaseUrl, Repo, EmptyConfigDigest, EmptyConfigBlob, Token) of
        ok ->
            %% Build manifest using ocibuild_sbom
            Manifest = ocibuild_sbom:build_referrer_manifest(
                ArtifactDigest, ArtifactSize, SubjectDigest, SubjectSize
            ),
            ManifestJson = ocibuild_json:encode(Manifest),
            ManifestDigest = ocibuild_digest:sha256(ManifestJson),

            %% Push the manifest (using digest as tag - required for referrers)
            Url = io_lib:format(
                "~s/v2/~s/manifests/~s",
                [BaseUrl, binary_to_list(Repo), binary_to_list(ManifestDigest)]
            ),
            Headers =
                auth_headers(Token) ++
                    [{"Content-Type", "application/vnd.oci.image.manifest.v1+json"}],

            case ?MODULE:http_put(lists:flatten(Url), Headers, ManifestJson) of
                {ok, _} ->
                    ok;
                {error, {http_error, 404, _}} ->
                    {error, {referrer_not_supported, SubjectDigest}};
                {error, {http_error, 405, _}} ->
                    {error, {referrer_not_supported, SubjectDigest}};
                {error, {http_error, 400, Body}} ->
                    case is_unsupported_manifest_error(Body) of
                        true -> {error, {referrer_not_supported, SubjectDigest}};
                        false -> {error, {push_referrer_manifest, Body}}
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% @private Check if error body indicates unsupported manifest type
-spec is_unsupported_manifest_error(binary() | string()) -> boolean().
is_unsupported_manifest_error(Body) when is_binary(Body) ->
    is_unsupported_manifest_error(binary_to_list(Body));
is_unsupported_manifest_error(Body) when is_list(Body) ->
    LowerBody = string:lowercase(Body),
    %% Check for specific error messages indicating referrer/artifact support is missing
    %% Note: "artifact" alone is too broad and may cause false positives
    string:find(LowerBody, "unsupported") =/= nomatch orelse
        string:find(LowerBody, "not supported") =/= nomatch orelse
        string:find(LowerBody, "unknown manifest") =/= nomatch.

%%%===================================================================
%%% Cosign Signature Push
%%%===================================================================

-doc """
Push a cosign-compatible signature as a referrer to an image manifest.

The signature is attached using the OCI referrers API with:
- artifactType: application/vnd.dev.cosign.simplesigning.v1+json
- Signature stored as base64 in layer annotation
- Payload stored as config blob

Returns ok on success, or {error, {referrer_not_supported, _}} if the
registry doesn't support the referrers API.
""".
-spec push_signature(
    PayloadJson :: binary(),
    Signature :: binary(),
    Registry :: binary(),
    Repo :: binary(),
    SubjectDigest :: binary(),
    SubjectSize :: non_neg_integer(),
    Auth :: map()
) -> ok | {error, term()}.
push_signature(PayloadJson, Signature, Registry, Repo, SubjectDigest, SubjectSize, Auth) ->
    push_signature(PayloadJson, Signature, Registry, Repo, SubjectDigest, SubjectSize, Auth, #{}).

-doc "Push a cosign signature with options.".
-spec push_signature(
    PayloadJson :: binary(),
    Signature :: binary(),
    Registry :: binary(),
    Repo :: binary(),
    SubjectDigest :: binary(),
    SubjectSize :: non_neg_integer(),
    Auth :: map(),
    Opts :: push_opts()
) -> ok | {error, term()}.
push_signature(PayloadJson, Signature, Registry, Repo, SubjectDigest, SubjectSize, Auth, _Opts) ->
    BaseUrl = registry_url(Registry),
    NormalizedRepo = normalize_repo(Registry, Repo),

    %% Push requires read+write access
    case get_auth_token(Registry, NormalizedRepo, Auth, push) of
        {ok, Token} ->
            %% Push the payload blob (simplesigning JSON)
            PayloadDigest = ocibuild_digest:sha256(PayloadJson),
            PayloadSize = byte_size(PayloadJson),

            case push_blob(BaseUrl, NormalizedRepo, PayloadDigest, PayloadJson, Token) of
                ok ->
                    %% Build and push the signature referrer manifest
                    push_signature_manifest(
                        BaseUrl,
                        NormalizedRepo,
                        Token,
                        PayloadDigest,
                        PayloadSize,
                        Signature,
                        SubjectDigest,
                        SubjectSize
                    );
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% @private Build and push a cosign signature manifest using tag scheme.
%% Cosign uses tag-based discovery by default: sha256-<hex>.sig
%% See: https://github.com/sigstore/cosign/blob/main/pkg/oci/remote/remote.go
-spec push_signature_manifest(
    BaseUrl :: string(),
    Repo :: binary(),
    Token :: term(),
    PayloadDigest :: binary(),
    PayloadSize :: non_neg_integer(),
    Signature :: binary(),
    SubjectDigest :: binary(),
    SubjectSize :: non_neg_integer()
) -> ok | {error, term()}.
push_signature_manifest(
    BaseUrl, Repo, Token, PayloadDigest, PayloadSize, Signature, SubjectDigest, _SubjectSize
) ->
    %% Build manifest using ocibuild_sign (for cosign compatibility)
    Manifest = ocibuild_sign:build_signature_manifest(PayloadDigest, PayloadSize, Signature),
    ManifestJson = ocibuild_json:encode(Manifest),

    %% Cosign tag scheme: sha256-<hex>.sig
    %% SubjectDigest is like "sha256:bb7294..." -> tag is "sha256-bb7294....sig"
    SignatureTag = digest_to_signature_tag(SubjectDigest),

    %% Push the manifest to the signature tag
    Url = io_lib:format(
        "~s/v2/~s/manifests/~s",
        [BaseUrl, binary_to_list(Repo), binary_to_list(SignatureTag)]
    ),
    Headers =
        auth_headers(Token) ++
            [{"Content-Type", "application/vnd.oci.image.manifest.v1+json"}],

    case ?MODULE:http_put(lists:flatten(Url), Headers, ManifestJson) of
        {ok, _} ->
            ok;
        {error, _} = Err ->
            Err
    end.

%% @private Convert a digest to cosign signature tag format.
%% sha256:abc123... -> sha256-abc123....sig
-spec digest_to_signature_tag(binary()) -> binary().
digest_to_signature_tag(Digest) ->
    %% Replace : with - and append .sig
    case binary:split(Digest, <<":">>) of
        [Algorithm, Hex] ->
            <<Algorithm/binary, "-", Hex/binary, ".sig">>;
        _ ->
            %% Fallback: just append .sig
            <<Digest/binary, ".sig">>
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Get the base URL for a registry
-spec registry_url(binary()) -> string().
registry_url(Registry) ->
    case maps:find(Registry, ?REGISTRY_URLS) of
        {ok, Url} ->
            Url;
        _ ->
            "https://" ++ binary_to_list(Registry)
    end.

%% Normalize repository name for a given registry
%% Docker Hub requires library/ prefix for official images
-spec normalize_repo(binary(), binary()) -> binary().
normalize_repo(~"docker.io", Repo) ->
    normalize_docker_hub_repo(Repo);
normalize_repo(_Registry, Repo) ->
    Repo.

%% Check if a manifest is a manifest list (multi-platform image index)
-spec is_manifest_list(map()) -> boolean().
is_manifest_list(Manifest) ->
    case maps:find(~"mediaType", Manifest) of
        {ok, ~"application/vnd.oci.image.index.v1+json"} ->
            true;
        {ok, ~"application/vnd.docker.distribution.manifest.list.v2+json"} ->
            true;
        _ ->
            %% Also check for "manifests" key as fallback
            maps:is_key(~"manifests", Manifest) andalso
                not maps:is_key(~"config", Manifest)
    end.

%% Select the appropriate platform manifest from a manifest list
-spec select_platform_manifest(map()) -> {ok, binary()} | {error, term()}.
select_platform_manifest(#{~"manifests" := Manifests}) ->
    %% Determine target platform (default to current system or linux/amd64)
    {TargetOs, TargetArch} = get_target_platform(),

    %% Filter out attestation manifests and find matching platform
    CandidateManifests = [
        M
     || M <- Manifests,
        not is_attestation_manifest(M)
    ],

    case find_platform_manifest(CandidateManifests, TargetOs, TargetArch) of
        {ok, _} = Result ->
            Result;
        {error, no_matching_platform} ->
            %% Fall back to first non-attestation manifest
            case CandidateManifests of
                [First | _] ->
                    {ok, maps:get(~"digest", First)};
                [] ->
                    {error, no_manifests_available}
            end
    end;
select_platform_manifest(_) ->
    {error, not_a_manifest_list}.

%% Select a manifest for a specific platform
-spec select_platform_manifest(map(), ocibuild:platform()) -> {ok, binary()} | {error, term()}.
select_platform_manifest(#{~"manifests" := Manifests}, Platform) ->
    TargetOs = maps:get(os, Platform),
    TargetArch = maps:get(architecture, Platform),
    TargetVariant = maps:get(variant, Platform, undefined),

    %% Filter out attestation manifests
    CandidateManifests = [
        M
     || M <- Manifests,
        not is_attestation_manifest(M)
    ],

    find_platform_manifest_with_variant(CandidateManifests, TargetOs, TargetArch, TargetVariant);
select_platform_manifest(_, _) ->
    {error, not_a_manifest_list}.

%% Find a manifest matching the target platform with optional variant
-spec find_platform_manifest_with_variant([map()], binary(), binary(), binary() | undefined) ->
    {ok, binary()} | {error, no_matching_platform}.
find_platform_manifest_with_variant([], _TargetOs, _TargetArch, _TargetVariant) ->
    {error, no_matching_platform};
find_platform_manifest_with_variant(
    [#{~"platform" := Platform, ~"digest" := Digest} | Rest],
    TargetOs,
    TargetArch,
    TargetVariant
) ->
    %% Convert JSON platform to atom-keyed map for ocibuild_index
    PlatformMap = json_platform_to_map(Platform),
    TargetMap = make_target_platform(TargetOs, TargetArch, TargetVariant),
    case ocibuild_index:matches_platform(PlatformMap, TargetMap) of
        true ->
            {ok, Digest};
        false ->
            find_platform_manifest_with_variant(Rest, TargetOs, TargetArch, TargetVariant)
    end;
find_platform_manifest_with_variant([_ | Rest], TargetOs, TargetArch, TargetVariant) ->
    find_platform_manifest_with_variant(Rest, TargetOs, TargetArch, TargetVariant).

%% Convert JSON platform object to atom-keyed platform map
-spec json_platform_to_map(map()) -> ocibuild:platform().
json_platform_to_map(Platform) ->
    Base = #{
        os => maps:get(~"os", Platform, <<>>),
        architecture => maps:get(~"architecture", Platform, <<>>)
    },
    case maps:get(~"variant", Platform, undefined) of
        undefined -> Base;
        Variant -> Base#{variant => Variant}
    end.

%% Build a target platform map
-spec make_target_platform(binary(), binary(), binary() | undefined) -> ocibuild:platform().
make_target_platform(Os, Arch, undefined) ->
    #{os => Os, architecture => Arch};
make_target_platform(Os, Arch, Variant) ->
    #{os => Os, architecture => Arch, variant => Variant}.

%% Check if a manifest entry is an attestation (not a real image)
-spec is_attestation_manifest(map()) -> boolean().
is_attestation_manifest(#{~"annotations" := Annotations}) ->
    maps:is_key(~"vnd.docker.reference.type", Annotations);
is_attestation_manifest(#{~"platform" := #{~"architecture" := ~"unknown"}}) ->
    true;
is_attestation_manifest(_) ->
    false.

%% Find a manifest matching the target platform
-spec find_platform_manifest([map()], binary(), binary()) ->
    {ok, binary()} | {error, no_matching_platform}.
find_platform_manifest([], _TargetOs, _TargetArch) ->
    {error, no_matching_platform};
find_platform_manifest(
    [#{~"platform" := Platform, ~"digest" := Digest} | Rest],
    TargetOs,
    TargetArch
) ->
    Os = maps:get(~"os", Platform, <<>>),
    Arch = maps:get(~"architecture", Platform, <<>>),
    case {Os, Arch} of
        {TargetOs, TargetArch} ->
            {ok, Digest};
        _ ->
            find_platform_manifest(Rest, TargetOs, TargetArch)
    end;
find_platform_manifest([_ | Rest], TargetOs, TargetArch) ->
    find_platform_manifest(Rest, TargetOs, TargetArch).

%% Get the target platform (OS and architecture)
-spec get_target_platform() -> {binary(), binary()}.
get_target_platform() ->
    % Container images are always for Linux
    Os = ~"linux",
    Arch =
        case erlang:system_info(system_architecture) of
            Arch0 when is_list(Arch0) ->
                normalize_arch(list_to_binary(Arch0));
            Other ->
                logger:warning("Could not detect system architecture (~p), defaulting to amd64", [
                    Other
                ]),
                ~"amd64"
        end,
    {Os, Arch}.

%% Normalize architecture name to OCI format
-spec normalize_arch(binary()) -> binary().
normalize_arch(Arch) ->
    case Arch of
        <<"x86_64", _/binary>> -> ~"amd64";
        <<"amd64", _/binary>> -> ~"amd64";
        <<"aarch64", _/binary>> -> ~"arm64";
        <<"arm64", _/binary>> -> ~"arm64";
        <<"arm", _/binary>> -> ~"arm";
        % Default fallback
        _ -> ~"amd64"
    end.

%% Get authentication token with explicit scope
%% OpScope determines whether to request pull-only or pull+push access:
%% - pull: Only read access needed (for pulling base images)
%% - push: Read+write access needed (for pushing images)
-spec get_auth_token(binary(), binary(), map(), operation_scope()) ->
    {ok, binary() | {basic, binary()} | none} | {error, term()}.
%% Direct token takes priority for all registries
get_auth_token(_Registry, _Repo, #{token := Token}, _OpScope) ->
    {ok, Token};
%% Docker Hub with username/password requires token exchange via known auth server
get_auth_token(~"docker.io", Repo, #{username := _, password := _} = Auth, OpScope) ->
    docker_hub_auth(Repo, Auth, OpScope);
%% All other registries: discover auth via WWW-Authenticate challenge
get_auth_token(Registry, Repo, #{username := _, password := _} = Auth, OpScope) ->
    discover_auth(Registry, Repo, Auth, OpScope);
%% No auth provided - try anonymous access
get_auth_token(Registry, Repo, #{}, OpScope) ->
    discover_auth(Registry, Repo, #{}, OpScope).

%% Normalize Docker Hub repository name
%% Single-component names like "alpine" need "library/" prefix
-spec normalize_docker_hub_repo(binary()) -> binary().
normalize_docker_hub_repo(Repo) ->
    case binary:match(Repo, ~"/") of
        nomatch ->
            %% Official image - add library/ prefix
            <<"library/", Repo/binary>>;
        _ ->
            %% User/org image - use as-is
            Repo
    end.

%% Docker Hub specific authentication
%% OpScope determines whether to request pull-only or pull+push access
-spec docker_hub_auth(binary(), map(), operation_scope()) -> {ok, binary()} | {error, term()}.
docker_hub_auth(Repo, Auth, OpScope) ->
    %% Docker Hub requires getting a token from auth.docker.io
    %% Normalize repo name (add library/ prefix for official images)
    NormalizedRepo = normalize_docker_hub_repo(Repo),
    Scope = make_scope_string(NormalizedRepo, OpScope),
    Url =
        "https://auth.docker.io/token?service=registry.docker.io&scope=" ++
            encode_scope(Scope),

    Headers =
        case Auth of
            #{username := User, password := Pass} ->
                Encoded = base64:encode(<<User/binary, ":", Pass/binary>>),
                [{"Authorization", "Basic " ++ binary_to_list(Encoded)}];
            _ ->
                []
        end,

    case ?MODULE:http_get(Url, Headers) of
        {ok, Body} ->
            Response = ocibuild_json:decode(Body),
            case maps:find(~"token", Response) of
                {ok, Token} ->
                    {ok, Token};
                error ->
                    {error, no_token_in_response}
            end;
        {error, _} = Err ->
            Err
    end.

%% Discover authentication via WWW-Authenticate challenge (standard OCI flow)
%% 1. GET /v2/ to trigger 401 response with WWW-Authenticate header
%% 2. Parse the challenge to get realm, service, scope
%% 3. Exchange credentials at the realm for a Bearer token
%%
%% Note: Some registries (like GHCR) allow anonymous pull (returning 200 for /v2/)
%% but require authentication for push. When credentials are provided, we always
%% try to get a token using well-known endpoints for such registries.
%%
%% Wrapper for backward compatibility - defaults to push scope for existing callers
-spec discover_auth(binary(), binary(), map()) ->
    {ok, binary() | {basic, binary()} | none} | {error, term()}.
discover_auth(Registry, Repo, Auth) ->
    discover_auth(Registry, Repo, Auth, push).

%% Discover authentication with explicit scope parameter
%% OpScope determines whether to request pull-only or pull+push access
-spec discover_auth(binary(), binary(), map(), operation_scope()) ->
    {ok, binary() | {basic, binary()} | none} | {error, term()}.
discover_auth(Registry, Repo, Auth, OpScope) ->
    BaseUrl = registry_url(Registry),
    V2Url = BaseUrl ++ "/v2/",

    Profile = get_httpc_profile(),
    Request = {V2Url, [{"Connection", "close"}]},
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}, {ssl, ssl_opts()}],
    Opts = [{body_format, binary}, {socket_opts, [{keepalive, false}]}],

    case httpc:request(get, Request, HttpOpts, Opts, Profile) of
        {ok, {{_, 200, _}, RespHeaders, _}} ->
            %% Anonymous access allowed for /v2/, but push may still require auth
            %% If credentials provided, try to get a token anyway
            case Auth of
                #{username := _, password := _} ->
                    %% Try to get WWW-Authenticate from response (some registries include it)
                    %% Otherwise use well-known token endpoint
                    case get_www_authenticate(RespHeaders) of
                        {ok, WwwAuth} ->
                            handle_www_authenticate(WwwAuth, Repo, Auth, OpScope);
                        error ->
                            %% Use well-known token endpoint for registry
                            use_wellknown_token_endpoint(Registry, Repo, Auth, OpScope)
                    end;
                _ ->
                    {ok, none}
            end;
        {ok, {{_, 401, _}, RespHeaders, _}} ->
            %% Auth required - parse WWW-Authenticate challenge
            case get_www_authenticate(RespHeaders) of
                {ok, WwwAuth} ->
                    handle_www_authenticate(WwwAuth, Repo, Auth, OpScope);
                error ->
                    {error, no_www_authenticate_header}
            end;
        {ok, {{_, Status, Reason}, _, _}} ->
            {error, {http_error, Status, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Handle WWW-Authenticate header (Bearer or Basic)
%% OpScope determines whether to request pull-only or pull+push access
-spec handle_www_authenticate(string(), binary(), map(), operation_scope()) ->
    {ok, binary() | {basic, binary()} | none} | {error, term()}.
handle_www_authenticate(WwwAuth, Repo, Auth, OpScope) ->
    case parse_www_authenticate(WwwAuth) of
        {bearer, Challenge} ->
            exchange_token(Challenge, Repo, Auth, OpScope);
        basic ->
            case Auth of
                #{username := User, password := Pass} ->
                    Encoded = base64:encode(<<User/binary, ":", Pass/binary>>),
                    {ok, {basic, Encoded}};
                _ ->
                    {error, auth_required}
            end;
        unknown ->
            {error, {unknown_auth_scheme, WwwAuth}}
    end.

%% Well-known token endpoints for registries that allow anonymous /v2/ access
%% OpScope determines whether to request pull-only or pull+push access
-spec use_wellknown_token_endpoint(binary(), binary(), map(), operation_scope()) ->
    {ok, binary() | {basic, binary()}} | {error, term()}.
use_wellknown_token_endpoint(~"ghcr.io", Repo, Auth, OpScope) ->
    %% GHCR token endpoint - use standard OAuth2 token exchange
    Challenge = #{~"realm" => "https://ghcr.io/token", ~"service" => "ghcr.io"},
    exchange_token(Challenge, Repo, Auth, OpScope);
use_wellknown_token_endpoint(~"quay.io", Repo, Auth, OpScope) ->
    %% Quay.io token endpoint
    Challenge = #{~"realm" => "https://quay.io/v2/auth", ~"service" => "quay.io"},
    exchange_token(Challenge, Repo, Auth, OpScope);
use_wellknown_token_endpoint(Registry, _Repo, _Auth, _OpScope) ->
    %% Unknown registry - can't get token without WWW-Authenticate
    {error, {no_token_endpoint_for_registry, Registry}}.

%% Get WWW-Authenticate header from response
-spec get_www_authenticate([{string(), string()}]) -> {ok, string()} | error.
get_www_authenticate([]) ->
    error;
get_www_authenticate([{Key, Value} | Rest]) ->
    case string:lowercase(Key) of
        "www-authenticate" -> {ok, Value};
        _ -> get_www_authenticate(Rest)
    end.

%% Parse WWW-Authenticate header
%% Returns {bearer, #{~"realm" => ..., ~"service" => ...}} | basic | unknown
-spec parse_www_authenticate(string()) ->
    {bearer, #{binary() := string()}} | basic | unknown.
parse_www_authenticate(Header) ->
    %% Normalize to lowercase for scheme comparison
    case string:prefix(string:lowercase(Header), "bearer ") of
        nomatch ->
            case string:prefix(string:lowercase(Header), "basic") of
                nomatch -> unknown;
                _ -> basic
            end;
        _ ->
            %% Extract the params part (after "Bearer ")
            ParamsStr = string:slice(Header, 7),
            Params = parse_auth_params(ParamsStr),
            case maps:find(~"realm", Params) of
                {ok, Realm} ->
                    {bearer, Params#{~"realm" => Realm}};
                error ->
                    unknown
            end
    end.

%% Parse key="value" pairs from auth header
%% Uses binary keys to avoid atom table exhaustion from untrusted input
-spec parse_auth_params(string()) -> #{binary() => string()}.
parse_auth_params(Str) ->
    %% Split on comma, then parse each key="value" pair
    Parts = string:split(Str, ",", all),
    lists:foldl(
        fun(Part, Acc) ->
            case parse_auth_param(string:trim(Part)) of
                {Key, Value} -> Acc#{Key => Value};
                error -> Acc
            end
        end,
        #{},
        Parts
    ).

%% Parse a single key="value" pair
%% Returns binary key to avoid atom exhaustion from untrusted HTTP headers
-spec parse_auth_param(string()) -> {binary(), string()} | error.
parse_auth_param(Str) ->
    case string:split(Str, "=", leading) of
        [KeyStr, ValueStr] ->
            %% Use binary key instead of atom to prevent atom table exhaustion
            Key = list_to_binary(string:lowercase(string:trim(KeyStr))),
            %% Remove surrounding quotes if present
            Value = string:trim(ValueStr, both, "\""),
            {Key, Value};
        _ ->
            error
    end.

%% Exchange credentials for a Bearer token at the realm URL
%% OpScope determines whether to request pull-only or pull+push access
-spec exchange_token(#{binary() := string()}, binary(), map(), operation_scope()) ->
    {ok, binary()} | {error, term()}.
exchange_token(#{~"realm" := Realm} = Challenge, Repo, Auth, OpScope) ->
    %% Build token URL with query params
    Service = maps:get(~"service", Challenge, ""),
    Scope = make_scope_string(Repo, OpScope),

    %% Build query string
    QueryParts = [
        "service=" ++ uri_string:quote(Service),
        "scope=" ++ encode_scope(Scope)
    ],
    QueryString = string:join(QueryParts, "&"),

    %% Append query to realm URL
    TokenUrl =
        case string:find(Realm, "?") of
            nomatch -> Realm ++ "?" ++ QueryString;
            _ -> Realm ++ "&" ++ QueryString
        end,

    %% Add basic auth header if credentials provided
    Headers =
        case Auth of
            #{username := User, password := Pass} ->
                Encoded = base64:encode(<<User/binary, ":", Pass/binary>>),
                [{"Authorization", "Basic " ++ binary_to_list(Encoded)}];
            _ ->
                []
        end,

    case ?MODULE:http_get(TokenUrl, Headers) of
        {ok, Body} ->
            Response = ocibuild_json:decode(Body),
            %% Try "token" first (Docker/GHCR), then "access_token" (some registries)
            case maps:find(~"token", Response) of
                {ok, Token} ->
                    {ok, Token};
                error ->
                    case maps:find(~"access_token", Response) of
                        {ok, Token} ->
                            {ok, Token};
                        error ->
                            {error, no_token_in_response}
                    end
            end;
        {error, _Reason} = Err ->
            Err
    end.

%% Build scope string for token exchange based on operation type
%% pull: Only requests read access (for base image pulls)
%% push: Requests read/write access (for pushing images)
-spec make_scope_string(binary(), operation_scope()) -> string().
make_scope_string(Repo, pull) ->
    "repository:" ++ binary_to_list(Repo) ++ ":pull";
make_scope_string(Repo, push) ->
    "repository:" ++ binary_to_list(Repo) ++ ":pull,push".

%% Encode scope parameter for auth token requests
%% Uses uri_string:quote for proper encoding
-spec encode_scope(string()) -> string().
encode_scope(Scope) ->
    %% Use uri_string:quote which properly encodes all special characters
    %% except those allowed in query strings (unreserved chars + some sub-delims)
    uri_string:quote(Scope).

%% Build auth headers
-spec auth_headers(binary() | {basic, binary()} | none) -> [{string(), string()}].
auth_headers(none) ->
    [];
auth_headers({basic, Encoded}) ->
    [{"Authorization", "Basic " ++ binary_to_list(Encoded)}];
auth_headers(Token) when is_binary(Token) ->
    [{"Authorization", "Bearer " ++ binary_to_list(Token)}].

%% Push all layers (including base image layers) in parallel
-spec push_layers(ocibuild:image(), string(), binary(), binary(), push_opts()) ->
    ok | {error, term()}.
push_layers(Image, BaseUrl, Repo, Token, Opts) ->
    %% First push base image layers (if any) that don't already exist in target
    case push_base_layers(Image, BaseUrl, Repo, Token, Opts) of
        ok ->
            %% Then push our new layers in parallel
            push_app_layers_parallel(Image, BaseUrl, Repo, Token, Opts);
        {error, _} = Err ->
            Err
    end.

%% Push app layers in parallel with bounded concurrency
-spec push_app_layers_parallel(ocibuild:image(), string(), binary(), binary(), push_opts()) ->
    ok | {error, term()}.
push_app_layers_parallel(Image, BaseUrl, Repo, Token, Opts) ->
    Layers = maps:get(layers, Image, []),
    case Layers of
        [] ->
            ok;
        _ ->
            %% Layers are stored in reverse order, reverse for correct indexing
            ReversedLayers = lists:reverse(Layers),
            TotalLayers = length(ReversedLayers),
            Platform = maps:get(platform, Image, undefined),

            %% Create indexed list: [{1, Layer1}, {2, Layer2}, ...]
            IndexedLayers = lists:zip(lists:seq(1, TotalLayers), ReversedLayers),

            %% Check and upload all layers in parallel
            PushFn = fun({Index, #{digest := Digest, data := Data, size := Size} = Layer}) ->
                LayerType = maps:get(layer_type, Layer, undefined),
                Label = make_upload_label(Index, TotalLayers, Platform, LayerType),

                %% Check if blob already exists
                CheckUrl = io_lib:format(
                    "~s/v2/~s/blobs/~s",
                    [BaseUrl, binary_to_list(Repo), binary_to_list(Digest)]
                ),
                Headers = auth_headers(Token),

                case ?MODULE:http_head(lists:flatten(CheckUrl), Headers) of
                    {ok, _} ->
                        %% Layer already exists - print through progress manager
                        StatusMsg = iolist_to_binary([Label, ~": exists (skipped)"]),
                        ocibuild_progress:print_status(StatusMsg),
                        ok;
                    {error, _} ->
                        %% Need to upload - register progress bar
                        ProgressRef = ocibuild_progress:register_bar(#{
                            label => Label,
                            total => Size
                        }),

                        ProgressCallback = fun(Info) ->
                            BytesSent = maps:get(bytes_sent, Info, 0),
                            ocibuild_progress:update(ProgressRef, BytesSent)
                        end,

                        LayerOpts = Opts#{
                            layer_index => Index,
                            total_layers => TotalLayers,
                            progress => ProgressCallback
                        },
                        Result = do_push_blob(BaseUrl, Repo, Digest, Data, Token, LayerOpts),
                        ocibuild_progress:complete(ProgressRef),
                        case Result of
                            ok -> ok;
                            {error, Reason} -> error({upload_failed, Digest, Reason})
                        end
                end
            end,

            try
                %% All layers in parallel with supervised HTTP workers
                ocibuild_http:pmap(PushFn, IndexedLayers, ?MAX_CONCURRENT_UPLOADS),
                ok
            catch
                error:{upload_failed, Digest, Reason} ->
                    {error, {upload_failed, Digest, Reason}}
            end
    end.

%% Create a label for layer upload progress
-spec make_upload_label(
    pos_integer(), pos_integer(), ocibuild:platform() | undefined, atom() | undefined
) -> binary().
make_upload_label(Index, Total, undefined, undefined) ->
    iolist_to_binary(io_lib:format("Layer ~B/~B", [Index, Total]));
make_upload_label(Index, Total, undefined, LayerType) ->
    iolist_to_binary(io_lib:format("Layer ~B/~B (~s)", [Index, Total, LayerType]));
make_upload_label(Index, Total, Platform, undefined) ->
    Arch = get_platform_arch(Platform),
    iolist_to_binary(io_lib:format("Layer ~B/~B (~s)", [Index, Total, Arch]));
make_upload_label(Index, Total, Platform, LayerType) ->
    Arch = get_platform_arch(Platform),
    iolist_to_binary(io_lib:format("Layer ~B/~B (~s, ~s)", [Index, Total, LayerType, Arch])).

%% Push base image layers to target registry in parallel
-spec push_base_layers(ocibuild:image(), string(), binary(), binary(), push_opts()) ->
    ok | {error, term()}.
push_base_layers(Image, BaseUrl, Repo, Token, Opts) ->
    case maps:get(base_manifest, Image, undefined) of
        undefined ->
            %% No base image, nothing to do
            ok;
        BaseManifest ->
            BaseLayers = maps:get(~"layers", BaseManifest, []),
            case BaseLayers of
                [] ->
                    ok;
                _ ->
                    BaseRef = maps:get(base, Image, none),
                    BaseAuth = maps:get(auth, Image, #{}),
                    Platform = maps:get(platform, Image, undefined),
                    push_base_layers_parallel(
                        BaseLayers, BaseRef, BaseAuth, BaseUrl, Repo, Token, Opts, Platform
                    )
            end
    end.

%% Push base layers in parallel with bounded concurrency
-spec push_base_layers_parallel(
    list(),
    term(),
    map(),
    string(),
    binary(),
    binary(),
    push_opts(),
    ocibuild:platform() | undefined
) ->
    ok | {error, term()}.
push_base_layers_parallel(BaseLayers, BaseRef, BaseAuth, BaseUrl, Repo, Token, Opts, Platform) ->
    Headers = auth_headers(Token),
    TotalLayers = length(BaseLayers),

    %% Create indexed list with size info
    IndexedLayers = lists:zip(lists:seq(1, TotalLayers), BaseLayers),

    PushFn = fun({Index, Layer}) ->
        Digest = maps:get(~"digest", Layer),
        Size = maps:get(~"size", Layer, 0),

        %% Check if blob already exists in target registry
        CheckUrl = io_lib:format(
            "~s/v2/~s/blobs/~s",
            [BaseUrl, binary_to_list(Repo), binary_to_list(Digest)]
        ),

        case ?MODULE:http_head(lists:flatten(CheckUrl), Headers) of
            {ok, _} ->
                %% Layer already exists in target, skip
                ok;
            {error, _} ->
                %% Need to download from source and upload to target
                download_and_upload_layer_with_progress(
                    BaseRef,
                    BaseAuth,
                    Digest,
                    Size,
                    BaseUrl,
                    Repo,
                    Token,
                    Opts,
                    Index,
                    TotalLayers,
                    Platform
                )
        end
    end,

    try
        %% Upload base layers in parallel with supervised HTTP workers
        ocibuild_http:pmap(PushFn, IndexedLayers, ?MAX_CONCURRENT_UPLOADS),
        ok
    catch
        error:{base_layer_upload_failed, Digest, Reason} ->
            {error, {base_layer_upload_failed, Digest, Reason}}
    end.

-spec download_and_upload_layer_with_progress(
    term(),
    map(),
    binary(),
    non_neg_integer(),
    string(),
    binary(),
    binary(),
    push_opts(),
    pos_integer(),
    pos_integer(),
    ocibuild:platform() | undefined
) ->
    ok.
download_and_upload_layer_with_progress(
    none, _BaseAuth, Digest, _Size, _BaseUrl, _Repo, _Token, _Opts, _Index, _Total, _Platform
) ->
    error({base_layer_upload_failed, Digest, no_base_ref});
download_and_upload_layer_with_progress(
    {SrcRegistry, SrcRepo, _SrcRef},
    BaseAuth,
    Digest,
    Size,
    BaseUrl,
    Repo,
    Token,
    Opts,
    Index,
    TotalLayers,
    Platform
) ->
    %% Check cache first - layers from save operation may already be cached
    case ocibuild_cache:get(Digest) of
        {ok, CachedData} ->
            %% Layer is cached, just upload it
            Label = make_base_layer_label(Index, TotalLayers, Platform),
            ProgressRef = ocibuild_progress:register_bar(#{
                label => Label,
                total => Size
            }),
            ProgressCallback = fun(Info) ->
                BytesSent = maps:get(bytes_sent, Info, 0),
                ocibuild_progress:update(ProgressRef, BytesSent)
            end,
            LayerOpts = Opts#{progress => ProgressCallback},
            Result = push_blob(BaseUrl, Repo, Digest, CachedData, Token, LayerOpts),
            ocibuild_progress:complete(ProgressRef),
            case Result of
                ok -> ok;
                {error, Reason} -> error({base_layer_upload_failed, Digest, Reason})
            end;
        {error, _} ->
            %% Not in cache, download from source registry
            Label = make_base_layer_label(Index, TotalLayers, Platform),
            ProgressRef = ocibuild_progress:register_bar(#{
                label => Label,
                total => Size
            }),
            case pull_blob(SrcRegistry, SrcRepo, Digest, BaseAuth) of
                {ok, Data} ->
                    %% Cache for future use
                    _ = ocibuild_cache:put(Digest, Data),
                    %% Create progress callback for upload
                    ProgressCallback = fun(Info) ->
                        BytesSent = maps:get(bytes_sent, Info, 0),
                        ocibuild_progress:update(ProgressRef, BytesSent)
                    end,
                    LayerOpts = Opts#{progress => ProgressCallback},
                    Result = push_blob(BaseUrl, Repo, Digest, Data, Token, LayerOpts),
                    ocibuild_progress:complete(ProgressRef),
                    case Result of
                        ok -> ok;
                        {error, Reason} -> error({base_layer_upload_failed, Digest, Reason})
                    end;
                {error, Reason} ->
                    ocibuild_progress:complete(ProgressRef),
                    error({base_layer_upload_failed, Digest, Reason})
            end
    end.

%% Create a label for base layer transfer progress
-spec make_base_layer_label(pos_integer(), pos_integer(), ocibuild:platform() | undefined) ->
    binary().
make_base_layer_label(Index, Total, undefined) ->
    iolist_to_binary(io_lib:format("Base ~B/~B", [Index, Total]));
make_base_layer_label(Index, Total, Platform) ->
    Arch = get_platform_arch(Platform),
    iolist_to_binary(io_lib:format("Base ~B/~B (~s)", [Index, Total, Arch])).

%% Get architecture from platform map (handles both atom and binary keys)
-spec get_platform_arch(map()) -> binary().
get_platform_arch(#{~"architecture" := Arch}) -> Arch;
get_platform_arch(#{architecture := Arch}) when is_binary(Arch) -> Arch;
get_platform_arch(#{architecture := Arch}) when is_list(Arch) -> list_to_binary(Arch);
get_platform_arch(_) -> ~"unknown".

%% Push config and return its digest and size
-spec push_config(ocibuild:image(), string(), binary(), binary()) ->
    {ok, binary(), non_neg_integer()} | {error, term()}.
push_config(#{config := Config}, BaseUrl, Repo, Token) ->
    %% Reverse diff_ids and history which are stored in reverse order for O(1) append
    Rootfs = maps:get(~"rootfs", Config),
    DiffIds = maps:get(~"diff_ids", Rootfs, []),
    History = maps:get(~"history", Config, []),
    ExportConfig = Config#{
        ~"rootfs" => Rootfs#{~"diff_ids" => lists:reverse(DiffIds)},
        ~"history" => lists:reverse(History)
    },
    ConfigJson = ocibuild_json:encode(ExportConfig),
    Digest = ocibuild_digest:sha256(ConfigJson),
    case push_blob(BaseUrl, Repo, Digest, ConfigJson, Token) of
        ok ->
            {ok, Digest, byte_size(ConfigJson)};
        {error, _} = Err ->
            Err
    end.

%% Push a single blob (backward compatible version)
-spec push_blob(string(), binary(), binary(), binary(), binary()) -> ok | {error, term()}.
push_blob(BaseUrl, Repo, Digest, Data, Token) ->
    push_blob(BaseUrl, Repo, Digest, Data, Token, #{}).

%% Push a single blob with options (supports chunked uploads)
-spec push_blob(string(), binary(), binary(), binary(), binary(), push_opts()) ->
    ok | {error, term()}.
push_blob(BaseUrl, Repo, Digest, Data, Token, Opts) ->
    %% Check if blob already exists
    CheckUrl =
        io_lib:format(
            "~s/v2/~s/blobs/~s",
            [BaseUrl, binary_to_list(Repo), binary_to_list(Digest)]
        ),
    Headers = auth_headers(Token),

    case ?MODULE:http_head(lists:flatten(CheckUrl), Headers) of
        {ok, _} ->
            %% Blob already exists
            ok;
        {error, _} ->
            %% Need to upload
            do_push_blob(BaseUrl, Repo, Digest, Data, Token, Opts)
    end.

%% Actually upload a blob - decides between monolithic and chunked based on size
%% Falls back to monolithic if registry doesn't support chunked uploads (416 error)
-spec do_push_blob(string(), binary(), binary(), binary(), binary(), push_opts()) ->
    ok | {error, term()}.
do_push_blob(BaseUrl, Repo, Digest, Data, Token, Opts) ->
    ChunkSize = maps:get(chunk_size, Opts, ocibuild_adapter:default_chunk_size()),
    case byte_size(Data) >= ChunkSize of
        true ->
            %% Try chunked upload for large blobs
            case upload_blob_chunked(BaseUrl, Repo, Digest, Data, Token, Opts) of
                ok ->
                    ok;
                {error, {http_error, 416, _}} ->
                    %% Registry doesn't support chunked uploads (e.g., GHCR)
                    %% Fall back to monolithic upload
                    do_push_blob_monolithic(BaseUrl, Repo, Digest, Data, Token, Opts);
                {error, _} = Err ->
                    Err
            end;
        false ->
            %% Use monolithic upload for small blobs
            do_push_blob_monolithic(BaseUrl, Repo, Digest, Data, Token, Opts)
    end.

%% Monolithic upload - single PUT request
-spec do_push_blob_monolithic(string(), binary(), binary(), binary(), binary(), push_opts()) ->
    ok | {error, term()}.
do_push_blob_monolithic(BaseUrl, Repo, Digest, Data, Token, Opts) ->
    ProgressFn = maps:get(progress, Opts, undefined),
    LayerIndex = maps:get(layer_index, Opts, 0),
    TotalLayers = maps:get(total_layers, Opts, 1),
    TotalSize = byte_size(Data),
    %% Report initial progress (0%)
    maybe_report_progress(ProgressFn, #{
        phase => uploading,
        layer_index => LayerIndex,
        total_layers => TotalLayers,
        bytes_sent => 0,
        total_bytes => TotalSize
    }),
    %% Start upload session
    InitUrl = io_lib:format("~s/v2/~s/blobs/uploads/", [BaseUrl, binary_to_list(Repo)]),
    Headers = auth_headers(Token),

    case ?MODULE:http_post(lists:flatten(InitUrl), Headers, <<>>) of
        {ok, _, ResponseHeaders} ->
            %% Get upload location
            case proplists:get_value("location", ResponseHeaders) of
                undefined ->
                    {error, no_upload_location};
                Location ->
                    %% Complete upload with PUT
                    %% The Location header may be relative or absolute
                    AbsLocation = resolve_url(BaseUrl, Location),
                    %% Use ? if no query params exist, & otherwise
                    Separator =
                        case lists:member($?, AbsLocation) of
                            true -> "&";
                            false -> "?"
                        end,
                    PutUrl = AbsLocation ++ Separator ++ "digest=" ++ binary_to_list(Digest),
                    PutHeaders =
                        Headers ++
                            [
                                {"Content-Type", "application/octet-stream"},
                                {"Content-Length", integer_to_list(byte_size(Data))}
                            ],
                    %% Use longer timeout for large uploads: base 60s + 1s per 100KB
                    PutTimeout = 60000 + (byte_size(Data) div 100),
                    case ?MODULE:http_put(PutUrl, PutHeaders, Data, PutTimeout) of
                        {ok, _} ->
                            %% Report final progress (100%)
                            maybe_report_progress(ProgressFn, #{
                                phase => uploading,
                                layer_index => LayerIndex,
                                total_layers => TotalLayers,
                                bytes_sent => TotalSize,
                                total_bytes => TotalSize
                            }),
                            ok;
                        {error, _} = Err ->
                            Err
                    end
            end;
        {error, _} = Err ->
            Err
    end.

%%%===================================================================
%%% Chunked upload functions (OCI Distribution Spec compliant)
%%%===================================================================

%% Start a new upload session
%% POST /v2/{repo}/blobs/uploads/ -> returns Location header with upload URL
-spec start_upload_session(string(), binary(), binary()) ->
    {ok, upload_session()} | {error, term()}.
start_upload_session(BaseUrl, Repo, Token) ->
    InitUrl = io_lib:format("~s/v2/~s/blobs/uploads/", [BaseUrl, binary_to_list(Repo)]),
    Headers = auth_headers(Token),
    case ?MODULE:http_post(lists:flatten(InitUrl), Headers, <<>>) of
        {ok, _, ResponseHeaders} ->
            case proplists:get_value("location", ResponseHeaders) of
                undefined ->
                    {error, no_upload_location};
                Location ->
                    AbsLocation = resolve_url(BaseUrl, Location),
                    {ok, #{upload_url => AbsLocation, bytes_uploaded => 0}}
            end;
        {error, _} = Err ->
            Err
    end.

%% Upload a single chunk via PATCH
%% PATCH {upload_url} with Content-Range header
-spec upload_chunk(upload_session(), binary(), non_neg_integer(), non_neg_integer(), binary()) ->
    {ok, upload_session()} | {error, term()}.
upload_chunk(Session, ChunkData, RangeStart, RangeEnd, Token) ->
    #{upload_url := UploadUrl} = Session,
    Headers =
        auth_headers(Token) ++
            [
                {"Content-Type", "application/octet-stream"},
                {"Content-Range", format_content_range(RangeStart, RangeEnd)},
                {"Content-Length", integer_to_list(byte_size(ChunkData))}
            ],
    %% Use longer timeout for large chunks: base 60s + 1ms per 100 bytes
    Timeout = max(?DEFAULT_TIMEOUT, 60000 + (byte_size(ChunkData) div 100)),
    case ?MODULE:http_patch(UploadUrl, Headers, ChunkData, Timeout) of
        {ok, 202, ResponseHeaders} ->
            %% Parse Range header to get bytes uploaded
            BytesUploaded =
                case proplists:get_value("range", ResponseHeaders) of
                    undefined ->
                        %% If no Range header, assume all bytes were accepted
                        RangeEnd + 1;
                    RangeValue ->
                        case parse_range_header(RangeValue) of
                            {ok, Bytes} -> Bytes;
                            error -> RangeEnd + 1
                        end
                end,
            %% Update session with new upload URL if provided (some registries change it)
            NewUploadUrl =
                case proplists:get_value("location", ResponseHeaders) of
                    undefined -> UploadUrl;
                    NewLoc -> resolve_url(UploadUrl, NewLoc)
                end,
            {ok, Session#{upload_url => NewUploadUrl, bytes_uploaded => BytesUploaded}};
        {ok, Status, _} ->
            {error, {http_error, Status, "Unexpected status from PATCH"}};
        {error, _} = Err ->
            Err
    end.

%% Complete the upload via PUT
%% PUT {upload_url}?digest={digest} with optional final chunk
-spec complete_upload(upload_session(), binary(), binary(), binary()) ->
    ok | {error, term()}.
complete_upload(Session, Digest, Token, FinalChunk) ->
    #{upload_url := UploadUrl} = Session,
    Separator =
        case lists:member($?, UploadUrl) of
            true -> "&";
            false -> "?"
        end,
    PutUrl = UploadUrl ++ Separator ++ "digest=" ++ binary_to_list(Digest),
    Headers =
        auth_headers(Token) ++
            [
                {"Content-Type", "application/octet-stream"},
                {"Content-Length", integer_to_list(byte_size(FinalChunk))}
            ],
    %% Use adaptive timeout: base 60s + 1ms per 100 bytes
    PutTimeout = max(?DEFAULT_TIMEOUT, 60000 + (byte_size(FinalChunk) div 100)),
    case ?MODULE:http_put(PutUrl, Headers, FinalChunk, PutTimeout) of
        {ok, _} ->
            ok;
        {error, _} = Err ->
            Err
    end.

%% Main chunked upload orchestration
%% Uploads blob in chunks via PATCH requests, then completes with PUT
-spec upload_blob_chunked(string(), binary(), binary(), binary(), binary(), push_opts()) ->
    ok | {error, term()}.
upload_blob_chunked(BaseUrl, Repo, Digest, Data, Token, Opts) ->
    ChunkSize = maps:get(chunk_size, Opts, ocibuild_adapter:default_chunk_size()),
    TotalSize = byte_size(Data),
    ProgressFn = maps:get(progress, Opts, undefined),
    LayerIndex = maps:get(layer_index, Opts, 0),
    TotalLayers = maps:get(total_layers, Opts, 1),

    %% Start upload session
    case start_upload_session(BaseUrl, Repo, Token) of
        {ok, Session} ->
            %% Upload chunks using state record
            State = #upload_state{
                session = Session,
                data = Data,
                digest = Digest,
                offset = 0,
                chunk_size = ChunkSize,
                total_size = TotalSize,
                token = Token,
                progress_fn = ProgressFn,
                layer_index = LayerIndex,
                total_layers = TotalLayers
            },
            upload_chunks_loop(State);
        {error, _} = Err ->
            Err
    end.

%% Internal: loop through chunks and upload each one
-spec upload_chunks_loop(#upload_state{}) -> ok | {error, term()}.
upload_chunks_loop(
    #upload_state{
        session = Session,
        data = Data,
        digest = Digest,
        offset = Offset,
        chunk_size = ChunkSize,
        total_size = TotalSize,
        token = Token,
        progress_fn = ProgressFn,
        layer_index = LayerIndex,
        total_layers = TotalLayers
    } = State
) ->
    Remaining = TotalSize - Offset,
    case Remaining =< ChunkSize of
        true ->
            %% Final chunk - send via PUT to complete the upload
            FinalChunk = binary:part(Data, Offset, Remaining),
            %% Report final progress
            maybe_report_progress(ProgressFn, #{
                phase => uploading,
                layer_index => LayerIndex,
                total_layers => TotalLayers,
                bytes_sent => TotalSize,
                total_bytes => TotalSize
            }),
            complete_upload(Session, Digest, Token, FinalChunk);
        false ->
            %% Upload chunk via PATCH
            ChunkData = binary:part(Data, Offset, ChunkSize),
            RangeStart = Offset,
            RangeEnd = Offset + ChunkSize - 1,

            %% Report progress before chunk upload
            maybe_report_progress(ProgressFn, #{
                phase => uploading,
                layer_index => LayerIndex,
                total_layers => TotalLayers,
                bytes_sent => Offset,
                total_bytes => TotalSize
            }),

            case
                with_retry(
                    fun() -> upload_chunk(Session, ChunkData, RangeStart, RangeEnd, Token) end, 3
                )
            of
                {ok, NewSession} ->
                    %% Continue with next chunk
                    upload_chunks_loop(State#upload_state{
                        session = NewSession,
                        offset = Offset + ChunkSize
                    });
                {error, _} = Err ->
                    Err
            end
    end.

%% Push manifest
%%
%% Opts: config_digest, config_size
-spec push_manifest(
    Image :: ocibuild:image(),
    BaseUrl :: string(),
    Repo :: binary(),
    Tag :: binary(),
    Token :: binary(),
    Opts :: map()
) ->
    {ok, Digest :: binary()} | {error, term()}.
push_manifest(Image, BaseUrl, Repo, Tag, Token, Opts) ->
    ConfigDigest = maps:get(config_digest, Opts),
    ConfigSize = maps:get(config_size, Opts),
    %% Get base image layers if present (these already exist in registry)
    BaseLayerDescriptors =
        case maps:get(base_manifest, Image, undefined) of
            undefined ->
                [];
            BaseManifest ->
                %% Base manifest layers are already in the correct format
                maps:get(~"layers", BaseManifest, [])
        end,

    %% Our layers are stored in reverse order, reverse for correct manifest order
    NewLayerDescriptors =
        [
            #{
                ~"mediaType" => MediaType,
                ~"digest" => Digest,
                ~"size" => Size
            }
         || #{
                media_type := MediaType,
                digest := Digest,
                size := Size
            } <-
                lists:reverse(maps:get(layers, Image, []))
        ],

    %% Combine: base layers first, then our new layers
    LayerDescriptors = BaseLayerDescriptors ++ NewLayerDescriptors,

    %% Get annotations from image
    Annotations = maps:get(annotations, Image, #{}),

    {ManifestJson, ManifestDigest} =
        ocibuild_manifest:build(
            #{
                ~"mediaType" =>
                    ~"application/vnd.oci.image.config.v1+json",
                ~"digest" => ConfigDigest,
                ~"size" => ConfigSize
            },
            LayerDescriptors,
            Annotations
        ),

    Url = io_lib:format(
        "~s/v2/~s/manifests/~s",
        [BaseUrl, binary_to_list(Repo), binary_to_list(Tag)]
    ),
    Headers =
        auth_headers(Token) ++ [{"Content-Type", "application/vnd.oci.image.manifest.v1+json"}],

    case ?MODULE:http_put(lists:flatten(Url), Headers, ManifestJson) of
        {ok, _} ->
            {ok, ManifestDigest};
        {error, _} = Err ->
            Err
    end.

%%%===================================================================
%%% URL helpers
%%%===================================================================

%% Resolve a potentially relative URL against a base URL
%% Returns absolute URL suitable for httpc
-spec resolve_url(string(), string()) -> string().
resolve_url(_BaseUrl, [$h, $t, $t, $p, $s, $: | _] = AbsUrl) ->
    %% Already absolute (starts with https:)
    AbsUrl;
resolve_url(_BaseUrl, [$h, $t, $t, $p, $: | _] = AbsUrl) ->
    %% Already absolute (starts with http:)
    AbsUrl;
resolve_url(BaseUrl, [$/ | _] = RelPath) ->
    %% Relative path - extract scheme and host from BaseUrl
    case uri_string:parse(BaseUrl) of
        #{scheme := Scheme, host := Host, port := Port} ->
            lists:flatten(io_lib:format("~s://~s:~B~s", [Scheme, Host, Port, RelPath]));
        #{scheme := Scheme, host := Host} ->
            lists:flatten(io_lib:format("~s://~s~s", [Scheme, Host, RelPath]));
        _ ->
            %% Fallback: prepend base URL
            BaseUrl ++ RelPath
    end;
resolve_url(BaseUrl, RelUrl) ->
    %% Assume relative URL without leading slash, append to base
    BaseUrl ++ "/" ++ RelUrl.

%%%===================================================================
%%% HTTP helpers (using httpc)
%%%===================================================================

%% Get the httpc profile to use for HTTP requests.
%% In worker context (spawned by ocibuild_http_worker), returns the worker's profile.
%% In fallback context (tests, direct API usage), uses the default profile.
-spec get_httpc_profile() -> atom().
get_httpc_profile() ->
    case get(ocibuild_httpc_profile) of
        undefined ->
            %% Fallback for non-worker context (tests, direct API usage)
            ensure_fallback_httpc(),
            ?DEFAULT_HTTPC_PROFILE;
        Profile ->
            Profile
    end.

%% Ensure ssl is started and fallback httpc is running (for non-worker context)
-spec ensure_fallback_httpc() -> ok.
ensure_fallback_httpc() ->
    %% SSL is needed for HTTPS
    case ssl:start() of
        ok -> ok;
        {error, {already_started, _}} -> ok
    end,
    %% Start httpc in stand_alone mode (not supervised by inets)
    %% This is the fallback for non-worker context (tests, direct API usage)
    ProfileName = httpc_profile_name(?DEFAULT_HTTPC_PROFILE),
    case persistent_term:get(?HTTPC_KEY, undefined) of
        undefined ->
            %% Check if a process is already registered under the profile name
            %% (e.g., from a previous crashed instance or test scenario)
            case whereis(ProfileName) of
                ExistingPid when is_pid(ExistingPid) ->
                    %% Reuse existing registered process
                    persistent_term:put(?HTTPC_KEY, ExistingPid),
                    ok;
                undefined ->
                    {ok, Pid} = inets:start(httpc, [{profile, ?DEFAULT_HTTPC_PROFILE}], stand_alone),
                    %% Register the process so httpc:request/5 can find it by profile name
                    %% stand_alone mode doesn't register automatically
                    true = register(ProfileName, Pid),
                    persistent_term:put(?HTTPC_KEY, Pid),
                    ok
            end;
        _Pid ->
            ok
    end.

%% Generate the registered name for an httpc profile (same as OTP internal)
-spec httpc_profile_name(atom()) -> atom().
httpc_profile_name(Profile) ->
    list_to_atom("httpc_" ++ atom_to_list(Profile)).

-doc """
Stop the fallback httpc to allow clean VM exit.

Note: When using the supervised HTTP pool (ocibuild_http), this is not needed
as cleanup happens automatically through OTP supervision.
""".
-spec stop_httpc() -> ok.
stop_httpc() ->
    %% First stop the supervised HTTP pool if running
    ocibuild_http:stop(),
    %% Then clean up fallback httpc if running
    case persistent_term:get(?HTTPC_KEY, undefined) of
        undefined ->
            ok;
        HttpcPid ->
            %% Clear the persistent_term first so no new requests use this pid
            _ = persistent_term:erase(?HTTPC_KEY),
            %% Spawn an UNLINKED process to do cleanup, with trap_exit to contain
            %% shutdown signals from inets:stop. This prevents EXIT messages from
            %% propagating to the caller/shell.
            CleanupPid = spawn(fun() ->
                process_flag(trap_exit, true),
                _ = (catch unregister(httpc_profile_name(?DEFAULT_HTTPC_PROFILE))),
                _ = (catch inets:stop(stand_alone, HttpcPid))
            end),
            Ref = erlang:monitor(process, CleanupPid),
            receive
                {'DOWN', Ref, process, CleanupPid, _} -> ok
            after ?DEFAULT_TIMEOUT ->
                %% Cleanup timed out - kill both the cleanup process AND httpc itself
                %% This ensures the httpc process (which owns TCP sockets) doesn't keep
                %% the VM alive. Without killing HttpcPid, the VM may hang on shutdown.
                exit(CleanupPid, kill),
                exit(HttpcPid, kill),
                erlang:demonitor(Ref, [flush]),
                ok
            end
    end.

%% Get SSL options for HTTPS requests
-spec ssl_opts() -> [ssl:tls_client_option()].
ssl_opts() ->
    %% Use system CA certificates (OTP 25+) with proper verification
    [
        {verify, verify_peer},
        {cacerts, public_key:cacerts_get()},
        {depth, 10},
        {customize_hostname_check, [
            {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
        ]}
    ].

-spec http_get(string(), [{string(), string()}]) -> {ok, binary()} | {error, term()}.
http_get(Url, Headers) ->
    http_get(Url, Headers, 5).

-spec http_get(string(), [{string(), string()}], non_neg_integer()) ->
    {ok, binary()} | {error, term()}.
http_get(_Url, _Headers, 0) ->
    {error, too_many_redirects};
http_get(Url, Headers, RedirectsLeft) ->
    Profile = get_httpc_profile(),
    %% Add Connection: close to prevent stale connection reuse issues
    AllHeaders = Headers ++ [{"Connection", "close"}],
    Request = {Url, AllHeaders},
    %% Disable autoredirect - we handle redirects manually to strip auth headers
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}, {autoredirect, false}, {ssl, ssl_opts()}],
    Opts = [{body_format, binary}, {socket_opts, [{keepalive, false}]}],
    case httpc:request(get, Request, HttpOpts, Opts, Profile) of
        {ok, {{_, Status, _}, _, Body}} when Status >= 200, Status < 300 ->
            {ok, Body};
        {ok, {{_, Status, _}, RespHeaders, _}} when
            Status =:= 301;
            Status =:= 302;
            Status =:= 303;
            Status =:= 307;
            Status =:= 308
        ->
            %% Handle redirect - strip Authorization header for external redirects
            case get_redirect_location(RespHeaders) of
                {ok, RedirectUrl} ->
                    %% Don't forward auth headers to external hosts (e.g., S3)
                    RedirectHeaders = strip_auth_headers(Headers),
                    http_get(RedirectUrl, RedirectHeaders, RedirectsLeft - 1);
                error ->
                    {error, {redirect_without_location, Status}}
            end;
        {ok, {{_, Status, Reason}, _, _}} ->
            {error, {http_error, Status, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Get Location header from response headers
-spec get_redirect_location([{string(), string()}]) -> {ok, string()} | error.
get_redirect_location([]) ->
    error;
get_redirect_location([{Key, Value} | Rest]) ->
    case string:lowercase(Key) of
        "location" -> {ok, Value};
        _ -> get_redirect_location(Rest)
    end.

%% Strip authorization headers for redirects to external hosts
-spec strip_auth_headers([{string(), string()}]) -> [{string(), string()}].
strip_auth_headers(Headers) ->
    [
        {K, V}
     || {K, V} <- Headers,
        string:lowercase(K) =/= "authorization"
    ].

-spec http_head(string(), [{string(), string()}]) ->
    {ok, [{string(), string()}]} | {error, term()}.
http_head(Url, Headers) ->
    Profile = get_httpc_profile(),
    AllHeaders = Headers ++ [{"Connection", "close"}],
    Request = {Url, AllHeaders},
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}, {ssl, ssl_opts()}],
    Opts = [{socket_opts, [{keepalive, false}]}],
    case httpc:request(head, Request, HttpOpts, Opts, Profile) of
        {ok, {{_, Status, _}, ResponseHeaders, _}} when Status >= 200, Status < 300 ->
            {ok, ResponseHeaders};
        {ok, {{_, Status, Reason}, _, _}} ->
            {error, {http_error, Status, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec http_post(string(), [{string(), string()}], binary()) ->
    {ok, binary(), [{string(), string()}]} | {error, term()}.
http_post(Url, Headers, Body) ->
    Profile = get_httpc_profile(),
    ContentType = proplists:get_value("Content-Type", Headers, "application/octet-stream"),
    AllHeaders = Headers ++ [{"Connection", "close"}],
    Request = {Url, AllHeaders, ContentType, Body},
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}, {ssl, ssl_opts()}],
    Opts = [{body_format, binary}, {socket_opts, [{keepalive, false}]}],
    case httpc:request(post, Request, HttpOpts, Opts, Profile) of
        {ok, {{_, Status, _}, ResponseHeaders, ResponseBody}} when Status >= 200, Status < 300 ->
            {ok, ResponseBody, normalize_headers(ResponseHeaders)};
        {ok, {{_, Status, Reason}, _, _ResponseBody}} ->
            {error, {http_error, Status, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec http_put(string(), [{string(), string()}], binary()) ->
    {ok, binary()} | {error, term()}.
http_put(Url, Headers, Body) ->
    http_put(Url, Headers, Body, ?DEFAULT_TIMEOUT).

-spec http_put(string(), [{string(), string()}], binary(), pos_integer()) ->
    {ok, binary()} | {error, term()}.
http_put(Url, Headers, Body, Timeout) ->
    Profile = get_httpc_profile(),
    ContentType = proplists:get_value("Content-Type", Headers, "application/octet-stream"),
    AllHeaders = Headers ++ [{"Connection", "close"}],
    Request = {Url, AllHeaders, ContentType, Body},
    HttpOpts = [{timeout, Timeout}, {ssl, ssl_opts()}],
    Opts = [{body_format, binary}, {socket_opts, [{keepalive, false}]}],
    case httpc:request(put, Request, HttpOpts, Opts, Profile) of
        {ok, {{_, Status, _}, _, ResponseBody}} when Status >= 200, Status < 300 ->
            {ok, ResponseBody};
        {ok, {{_, Status, Reason}, _, _}} ->
            {error, {http_error, Status, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

%% HTTP PATCH request for chunked uploads
%% Returns status code and headers for Content-Range tracking
-spec http_patch(string(), [{string(), string()}], binary(), pos_integer()) ->
    {ok, integer(), [{string(), string()}]} | {error, term()}.
http_patch(Url, Headers, Body, Timeout) ->
    Profile = get_httpc_profile(),
    ContentType = proplists:get_value("Content-Type", Headers, "application/octet-stream"),
    AllHeaders = Headers ++ [{"Connection", "close"}],
    Request = {Url, AllHeaders, ContentType, Body},
    HttpOpts = [{timeout, Timeout}, {ssl, ssl_opts()}],
    Opts = [{body_format, binary}, {socket_opts, [{keepalive, false}]}],
    case httpc:request(patch, Request, HttpOpts, Opts, Profile) of
        {ok, {{_, Status, _}, ResponseHeaders, _}} when Status >= 200, Status < 300 ->
            {ok, Status, normalize_headers(ResponseHeaders)};
        {ok, {{_, Status, Reason}, _, _}} ->
            {error, {http_error, Status, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Normalize headers to lowercase keys
-spec normalize_headers([{string(), string()}]) -> [{string(), string()}].
normalize_headers(Headers) ->
    [{string:lowercase(K), V} || {K, V} <- Headers].

%%%===================================================================
%%% Content-Range helpers for chunked uploads
%%%===================================================================

%% Format Content-Range header value for chunked upload
%% OCI spec uses format: "start-end" (inclusive, 0-indexed)
-spec format_content_range(non_neg_integer(), non_neg_integer()) -> string().
format_content_range(Start, End) when Start =< End ->
    lists:flatten(io_lib:format("~B-~B", [Start, End])).

%% Parse Range header from response (e.g., "0-1048575" -> 1048576 bytes uploaded)
%% Returns the number of bytes the server has received (end + 1)
-spec parse_range_header(string()) -> {ok, non_neg_integer()} | error.
parse_range_header(Value) ->
    case string:split(Value, "-") of
        [_StartStr, EndStr] ->
            case string:to_integer(string:trim(EndStr)) of
                {End, []} when End >= 0 ->
                    {ok, End + 1};
                _ ->
                    error
            end;
        _ ->
            error
    end.

%%%===================================================================
%%% Retry helpers
%%%===================================================================

-doc """
Check if an error is retriable (transient network/server issues).

Returns true for connection failures, timeouts, and server errors (5xx).
""".
-spec is_retriable_error(term()) -> boolean().
is_retriable_error({failed_connect, _}) -> true;
is_retriable_error(timeout) -> true;
is_retriable_error({http_error, 500, _}) -> true;
is_retriable_error({http_error, 502, _}) -> true;
is_retriable_error({http_error, 503, _}) -> true;
is_retriable_error({http_error, 504, _}) -> true;
% Rate limited
is_retriable_error({http_error, 429, _}) -> true;
is_retriable_error(closed) -> true;
is_retriable_error(econnreset) -> true;
is_retriable_error({error, econnreset}) -> true;
is_retriable_error(_) -> false.

-doc """
Execute a function with retry logic for transient failures.

Takes a zero-arity function that returns `{ok, Result}` or `{error, Reason}`.
Retries up to MaxRetries times with exponential backoff for retriable errors.

Example:
```
with_retry(fun() -> pull_blob(R, Repo, D, Auth, Opts) end, 3)
```
""".
-spec with_retry(fun(() -> {ok, T} | {error, term()}), non_neg_integer()) ->
    {ok, T} | {error, term()}
when
    T :: term().
with_retry(Fun, MaxRetries) ->
    with_retry(Fun, MaxRetries, MaxRetries).

-spec with_retry(fun(() -> {ok, T} | {error, term()}), non_neg_integer(), non_neg_integer()) ->
    {ok, T} | {error, term()}
when
    T :: term().
with_retry(Fun, _MaxRetries, 0) ->
    %% No retries left, make final attempt
    Fun();
with_retry(Fun, MaxRetries, RetriesLeft) ->
    case Fun() of
        {ok, _} = Success ->
            Success;
        {error, Reason} = Error ->
            case is_retriable_error(Reason) of
                true when RetriesLeft > 0 ->
                    %% Exponential backoff: 1s, 2s, 3s...
                    Delay = (MaxRetries - RetriesLeft + 1) * 1000,
                    timer:sleep(Delay),
                    with_retry(Fun, MaxRetries, RetriesLeft - 1);
                _ ->
                    Error
            end
    end.

%%%===================================================================
%%% Progress helpers
%%%===================================================================

%% Report progress if callback is defined
%% Wraps callback in try/catch to prevent user callback crashes from breaking operations
-spec maybe_report_progress(progress_callback() | undefined, progress_info()) -> ok.
maybe_report_progress(undefined, _Info) ->
    ok;
maybe_report_progress(ProgressFn, Info) when is_function(ProgressFn, 1) ->
    try
        ProgressFn(Info),
        ok
    catch
        _:_ ->
            %% Silently ignore callback errors to avoid disrupting progress display
            ok
    end.

%% HTTP GET with streaming progress support
-spec http_get_with_progress(
    string(),
    [{string(), string()}],
    progress_callback() | undefined,
    progress_phase(),
    non_neg_integer() | unknown,
    map()
) ->
    {ok, binary()} | {error, term()}.
http_get_with_progress(Url, Headers, undefined, _Phase, _TotalBytes, _ProgressOpts) ->
    %% No progress callback - use regular http_get
    ?MODULE:http_get(Url, Headers);
http_get_with_progress(Url, Headers, ProgressFn, Phase, TotalBytes, ProgressOpts) ->
    http_get_with_progress_internal(Url, Headers, ProgressFn, Phase, TotalBytes, ProgressOpts, 5).

-spec http_get_with_progress_internal(
    string(),
    [{string(), string()}],
    progress_callback(),
    progress_phase(),
    non_neg_integer() | unknown,
    map(),
    non_neg_integer()
) ->
    {ok, binary()} | {error, term()}.
http_get_with_progress_internal(_Url, _Headers, _ProgressFn, _Phase, _TotalBytes, _ProgressOpts, 0) ->
    {error, too_many_redirects};
http_get_with_progress_internal(
    Url, Headers, ProgressFn, Phase, TotalBytes, ProgressOpts, RedirectsLeft
) ->
    Profile = get_httpc_profile(),
    AllHeaders = Headers ++ [{"Connection", "close"}],
    Request = {Url, AllHeaders},
    %% Use streaming mode to receive chunks
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}, {autoredirect, false}, {ssl, ssl_opts()}],
    Opts = [
        {sync, false},
        {stream, self},
        {socket_opts, [{keepalive, false}]}
    ],
    LayerIndex = maps:get(layer_index, ProgressOpts, 0),
    TotalLayers = maps:get(total_layers, ProgressOpts, 1),

    case httpc:request(get, Request, HttpOpts, Opts, Profile) of
        {ok, RequestId} ->
            State = #stream_state{
                request_id = RequestId,
                progress_fn = ProgressFn,
                phase = Phase,
                total_bytes = TotalBytes,
                acc = <<>>,
                url = Url,
                headers = Headers,
                redirects_left = RedirectsLeft,
                layer_index = LayerIndex,
                total_layers = TotalLayers
            },
            receive_stream(State);
        {error, Reason} ->
            {error, Reason}
    end.

%% Receive streaming response chunks
-spec receive_stream(#stream_state{}) -> {ok, binary()} | {error, term()}.
receive_stream(
    #stream_state{
        request_id = RequestId,
        progress_fn = ProgressFn,
        phase = Phase,
        total_bytes = TotalBytes,
        acc = Acc,
        headers = Headers,
        redirects_left = RedirectsLeft,
        layer_index = LayerIndex,
        total_layers = TotalLayers
    } = State
) ->
    receive
        {http, {RequestId, stream_start, RespHeaders}} ->
            %% Got headers, try to get content length if not already known
            ActualTotal =
                case TotalBytes of
                    unknown ->
                        case get_content_length(RespHeaders) of
                            {ok, Len} -> Len;
                            error -> unknown
                        end;
                    _ ->
                        TotalBytes
                end,
            receive_stream(State#stream_state{total_bytes = ActualTotal});
        {http, {RequestId, stream, BinBodyPart}} ->
            NewAcc = <<Acc/binary, BinBodyPart/binary>>,
            BytesReceived = byte_size(NewAcc),
            maybe_report_progress(ProgressFn, #{
                phase => Phase,
                bytes_received => BytesReceived,
                total_bytes => TotalBytes,
                layer_index => LayerIndex,
                total_layers => TotalLayers
            }),
            receive_stream(State#stream_state{acc = NewAcc});
        {http, {RequestId, stream_end, _FinalHeaders}} ->
            %% Final progress report
            maybe_report_progress(ProgressFn, #{
                phase => Phase,
                bytes_received => byte_size(Acc),
                total_bytes => byte_size(Acc),
                layer_index => LayerIndex,
                total_layers => TotalLayers
            }),
            {ok, Acc};
        {http, {RequestId, {{_, Status, _}, _RespHeaders, Body}}} when
            Status >= 200, Status < 300
        ->
            %% Non-streaming response (small body)
            maybe_report_progress(ProgressFn, #{
                phase => Phase,
                bytes_received => byte_size(Body),
                total_bytes => byte_size(Body),
                layer_index => LayerIndex,
                total_layers => TotalLayers
            }),
            {ok, Body};
        {http, {RequestId, {{_, Status, _}, RespHeaders, _Body}}} when
            Status =:= 301; Status =:= 302; Status =:= 303; Status =:= 307; Status =:= 308
        ->
            %% Handle redirect
            case get_redirect_location(RespHeaders) of
                {ok, RedirectUrl} ->
                    RedirectHeaders = strip_auth_headers(Headers),
                    ProgressOpts = #{layer_index => LayerIndex, total_layers => TotalLayers},
                    http_get_with_progress_internal(
                        RedirectUrl,
                        RedirectHeaders,
                        ProgressFn,
                        Phase,
                        TotalBytes,
                        ProgressOpts,
                        RedirectsLeft - 1
                    );
                error ->
                    {error, {redirect_without_location, Status}}
            end;
        {http, {RequestId, {{_, Status, Reason}, _, _}}} ->
            {error, {http_error, Status, Reason}};
        {http, {RequestId, {error, Reason}}} ->
            {error, Reason}
    after 120000 ->
        httpc:cancel_request(RequestId),
        {error, timeout}
    end.

%% Get content length from response headers
-spec get_content_length([{string(), string()}]) -> {ok, non_neg_integer()} | error.
get_content_length([]) ->
    error;
get_content_length([{Key, Value} | Rest]) ->
    case string:lowercase(Key) of
        "content-length" ->
            try
                {ok, list_to_integer(Value)}
            catch
                _:_ ->
                    error
            end;
        _ ->
            get_content_length(Rest)
    end.

%% Sanitize HTTP error bodies to prevent credential leakage in logs/errors.
%% Registry error responses could potentially echo back credentials from requests.
%% This function extracts only safe, structured error information.
-spec sanitize_error_body(binary()) -> binary().
sanitize_error_body(Body) when is_binary(Body) ->
    %% Limit body size to prevent large payloads in error terms
    MaxSize = 500,
    TruncatedBody = case byte_size(Body) > MaxSize of
        true -> <<(binary:part(Body, 0, MaxSize))/binary, "... (truncated)">>;
        false -> Body
    end,
    %% Try to extract just the error message if it's JSON
    try
        case ocibuild_json:decode(TruncatedBody) of
            #{~"errors" := Errors} when is_list(Errors) ->
                %% OCI error format: {"errors": [{"code": "...", "message": "..."}]}
                SafeErrors = [extract_safe_error(E) || E <- Errors],
                ocibuild_json:encode(#{~"errors" => SafeErrors});
            #{~"error" := Error} when is_binary(Error) ->
                %% Simple error format
                ocibuild_json:encode(#{~"error" => Error});
            #{~"message" := Msg} when is_binary(Msg) ->
                ocibuild_json:encode(#{~"message" => Msg});
            _ ->
                %% Unknown JSON structure, redact potentially sensitive content
                redact_sensitive(TruncatedBody)
        end
    catch
        _:_ ->
            %% Not valid JSON, redact sensitive patterns
            redact_sensitive(TruncatedBody)
    end;
sanitize_error_body(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).

%% Extract safe fields from an OCI error object
-spec extract_safe_error(map()) -> map().
extract_safe_error(Error) when is_map(Error) ->
    SafeFields = [~"code", ~"message", ~"detail"],
    maps:filter(fun(K, _V) -> lists:member(K, SafeFields) end, Error);
extract_safe_error(_) ->
    #{}.

%% Redact strings that look like credentials
-spec redact_sensitive(binary()) -> binary().
redact_sensitive(Bin) ->
    %% Redact base64-encoded credentials (Authorization: Basic/Bearer patterns)
    %% and anything that looks like a token
    Patterns = [
        {<<"Basic [A-Za-z0-9+/=]+">>, <<"Basic [REDACTED]">>},
        {<<"Bearer [A-Za-z0-9._-]+">>, <<"Bearer [REDACTED]">>},
        {<<"token\":\"[^\"]+">>, <<"token\":\"[REDACTED]">>},
        {<<"password\":\"[^\"]+">>, <<"password\":\"[REDACTED]">>},
        {<<"secret\":\"[^\"]+">>, <<"secret\":\"[REDACTED]">>}
    ],
    lists:foldl(
        fun({Pattern, Replacement}, Acc) ->
            re:replace(Acc, Pattern, Replacement, [global, {return, binary}])
        end,
        Bin,
        Patterns
    ).

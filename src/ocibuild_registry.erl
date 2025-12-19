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
    pull_blob/3, pull_blob/4, pull_blob/5,
    push/5, push/6,
    check_blob_exists/4,
    stop_httpc/0
]).

%% Retry utilities (shared with ocibuild_layout)
-export([is_retriable_error/1, with_retry/2]).

%% Export internal HTTP functions (used via ?MODULE: for mockability in tests)
-export([http_get/2, http_head/2, http_post/3, http_patch/4, http_put/3, http_put/4]).

%% Export internal functions for testing
-export([push_blob/5, push_blob/6, format_content_range/2, parse_range_header/1]).

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
    auth => map()
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

-export_type([progress_callback/0, progress_info/0, pull_opts/0, push_opts/0, upload_session/0]).

-define(DEFAULT_TIMEOUT, 30000).
%% Default chunk size for chunked uploads: 5MB
-define(DEFAULT_CHUNK_SIZE, 5 * 1024 * 1024).
%% Stand-alone httpc profile for ocibuild (not supervised by inets)
-define(HTTPC_PROFILE, ocibuild).
-define(HTTPC_KEY, {?MODULE, httpc_pid}).
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

    %% Get auth token if needed
    case get_auth_token(Registry, NormalizedRepo, Auth) of
        {ok, Token} ->
            %% Fetch manifest (accept both single manifests and manifest lists)
            ManifestUrl =
                io_lib:format(
                    "~s/v2/~s/manifests/~s",
                    [BaseUrl, binary_to_list(NormalizedRepo), binary_to_list(Ref)]
                ),
            Headers =
                auth_headers(Token) ++
                    [
                        {"Accept", "application/vnd.oci.image.index.v1+json"},
                        {"Accept", "application/vnd.docker.distribution.manifest.list.v2+json"},
                        {"Accept", "application/vnd.oci.image.manifest.v1+json"},
                        {"Accept", "application/vnd.docker.distribution.manifest.v2+json"}
                    ],

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
                            case select_platform_manifest(Manifest) of
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
                                    ConfigSize
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
    pull_blob_internal(Registry, Repo, Digest, Auth, ProgressFn, layer, TotalBytes).

%% Internal blob pull with progress support
-spec pull_blob_internal(
    binary(),
    binary(),
    binary(),
    map(),
    progress_callback() | undefined,
    progress_phase(),
    non_neg_integer() | unknown
) ->
    {ok, binary()} | {error, term()}.
pull_blob_internal(Registry, Repo, Digest, Auth, ProgressFn, Phase, TotalBytes) ->
    BaseUrl = registry_url(Registry),
    NormalizedRepo = normalize_repo(Registry, Repo),

    case get_auth_token(Registry, NormalizedRepo, Auth) of
        {ok, Token} ->
            Url = io_lib:format(
                "~s/v2/~s/blobs/~s",
                [BaseUrl, binary_to_list(NormalizedRepo), binary_to_list(Digest)]
            ),
            Headers = auth_headers(Token),
            case
                http_get_with_progress(lists:flatten(Url), Headers, ProgressFn, Phase, TotalBytes)
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

    case get_auth_token(Registry, NormalizedRepo, Auth) of
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

-doc "Push an image to a registry.".
-spec push(ocibuild:image(), binary(), binary(), binary(), map()) -> ok | {error, term()}.
push(Image, Registry, Repo, Tag, Auth) ->
    push(Image, Registry, Repo, Tag, Auth, #{}).

-doc "Push an image to a registry with options (supports chunked uploads).".
-spec push(ocibuild:image(), binary(), binary(), binary(), map(), push_opts()) ->
    ok | {error, term()}.
push(Image, Registry, Repo, Tag, Auth, Opts) ->
    BaseUrl = registry_url(Registry),
    NormalizedRepo = normalize_repo(Registry, Repo),

    case get_auth_token(Registry, NormalizedRepo, Auth) of
        {ok, Token} ->
            %% Push layers
            case push_layers(Image, BaseUrl, NormalizedRepo, Token, Opts) of
                ok ->
                    %% Push config (no chunked upload needed - configs are small)
                    case push_config(Image, BaseUrl, NormalizedRepo, Token) of
                        {ok, ConfigDigest, ConfigSize} ->
                            %% Push manifest
                            push_manifest(
                                Image,
                                BaseUrl,
                                NormalizedRepo,
                                Tag,
                                Token,
                                ConfigDigest,
                                ConfigSize
                            );
                        {error, _} = Err ->
                            Err
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
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

%% Get authentication token
-spec get_auth_token(binary(), binary(), map()) ->
    {ok, binary() | {basic, binary()} | none} | {error, term()}.
%% Direct token takes priority for all registries
get_auth_token(_Registry, _Repo, #{token := Token}) ->
    {ok, Token};
%% Docker Hub with username/password requires token exchange via known auth server
get_auth_token(~"docker.io", Repo, #{username := _, password := _} = Auth) ->
    docker_hub_auth(Repo, Auth);
%% All other registries: discover auth via WWW-Authenticate challenge
get_auth_token(Registry, Repo, #{username := _, password := _} = Auth) ->
    discover_auth(Registry, Repo, Auth);
%% No auth provided - try anonymous access
get_auth_token(Registry, Repo, #{}) ->
    discover_auth(Registry, Repo, #{}).

%% Normalize Docker Hub repository name
%% Single-component names like "alpine" need "library/" prefix
-spec normalize_docker_hub_repo(binary()) -> binary().
normalize_docker_hub_repo(Repo) ->
    case binary:match(Repo, <<"/">>) of
        nomatch ->
            %% Official image - add library/ prefix
            <<"library/", Repo/binary>>;
        _ ->
            %% User/org image - use as-is
            Repo
    end.

%% Docker Hub specific authentication
-spec docker_hub_auth(binary(), map()) -> {ok, binary()} | {error, term()}.
docker_hub_auth(Repo, Auth) ->
    %% Docker Hub requires getting a token from auth.docker.io
    %% Normalize repo name (add library/ prefix for official images)
    NormalizedRepo = normalize_docker_hub_repo(Repo),
    Scope = "repository:" ++ binary_to_list(NormalizedRepo) ++ ":pull,push",
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
-spec discover_auth(binary(), binary(), map()) ->
    {ok, binary() | {basic, binary()} | none} | {error, term()}.
discover_auth(Registry, Repo, Auth) ->
    BaseUrl = registry_url(Registry),
    V2Url = BaseUrl ++ "/v2/",

    ensure_started(),
    Request = {V2Url, [{"Connection", "close"}]},
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}, {ssl, ssl_opts()}],
    Opts = [{body_format, binary}, {socket_opts, [{keepalive, false}]}],

    case httpc:request(get, Request, HttpOpts, Opts, ?HTTPC_PROFILE) of
        {ok, {{_, 200, _}, RespHeaders, _}} ->
            %% Anonymous access allowed for /v2/, but push may still require auth
            %% If credentials provided, try to get a token anyway
            case Auth of
                #{username := _, password := _} ->
                    %% Try to get WWW-Authenticate from response (some registries include it)
                    %% Otherwise use well-known token endpoint
                    case get_www_authenticate(RespHeaders) of
                        {ok, WwwAuth} ->
                            handle_www_authenticate(WwwAuth, Repo, Auth);
                        error ->
                            %% Use well-known token endpoint for registry
                            use_wellknown_token_endpoint(Registry, Repo, Auth)
                    end;
                _ ->
                    {ok, none}
            end;
        {ok, {{_, 401, _}, RespHeaders, _}} ->
            %% Auth required - parse WWW-Authenticate challenge
            case get_www_authenticate(RespHeaders) of
                {ok, WwwAuth} ->
                    handle_www_authenticate(WwwAuth, Repo, Auth);
                error ->
                    {error, no_www_authenticate_header}
            end;
        {ok, {{_, Status, Reason}, _, _}} ->
            {error, {http_error, Status, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Handle WWW-Authenticate header (Bearer or Basic)
-spec handle_www_authenticate(string(), binary(), map()) ->
    {ok, binary() | {basic, binary()} | none} | {error, term()}.
handle_www_authenticate(WwwAuth, Repo, Auth) ->
    case parse_www_authenticate(WwwAuth) of
        {bearer, Challenge} ->
            exchange_token(Challenge, Repo, Auth);
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
-spec use_wellknown_token_endpoint(binary(), binary(), map()) ->
    {ok, binary() | {basic, binary()}} | {error, term()}.
use_wellknown_token_endpoint(~"ghcr.io", _Repo, #{username := User, password := Pass}) ->
    %% GHCR: Use Basic Auth directly for push operations
    %% This works better with GITHUB_TOKEN than token exchange
    Encoded = base64:encode(<<User/binary, ":", Pass/binary>>),
    {ok, {basic, Encoded}};
use_wellknown_token_endpoint(~"ghcr.io", _Repo, #{}) ->
    %% No credentials for GHCR
    {ok, none};
use_wellknown_token_endpoint(~"quay.io", Repo, Auth) ->
    %% Quay.io token endpoint
    Challenge = #{~"realm" => "https://quay.io/v2/auth", ~"service" => "quay.io"},
    exchange_token(Challenge, Repo, Auth);
use_wellknown_token_endpoint(Registry, _Repo, _Auth) ->
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
%% Returns {bearer, #{<<"realm">> => ..., <<"service">> => ...}} | basic | unknown
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
-spec exchange_token(#{binary() := string()}, binary(), map()) ->
    {ok, binary()} | {error, term()}.
exchange_token(#{~"realm" := Realm} = Challenge, Repo, Auth) ->
    %% Build token URL with query params
    Service = maps:get(~"service", Challenge, ""),
    Scope = "repository:" ++ binary_to_list(Repo) ++ ":pull,push",

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

%% Push all layers (including base image layers)
-spec push_layers(ocibuild:image(), string(), binary(), binary(), push_opts()) ->
    ok | {error, term()}.
push_layers(Image, BaseUrl, Repo, Token, Opts) ->
    %% First push base image layers (if any) that don't already exist in target
    case push_base_layers(Image, BaseUrl, Repo, Token, Opts) of
        ok ->
            %% Then push our new layers
            Layers = maps:get(layers, Image, []),
            %% Layers are stored in reverse order, reverse for correct push order
            ReversedLayers = lists:reverse(Layers),
            TotalLayers = length(ReversedLayers),
            push_layers_with_index(ReversedLayers, BaseUrl, Repo, Token, Opts, 0, TotalLayers);
        {error, _} = Err ->
            Err
    end.

%% Push layers with index tracking for progress
-spec push_layers_with_index(
    [map()], string(), binary(), binary(), push_opts(), non_neg_integer(), non_neg_integer()
) ->
    ok | {error, term()}.
push_layers_with_index([], _BaseUrl, _Repo, _Token, _Opts, _Index, _Total) ->
    ok;
push_layers_with_index(
    [#{digest := Digest, data := Data} | Rest], BaseUrl, Repo, Token, Opts, Index, Total
) ->
    LayerOpts = Opts#{layer_index => Index, total_layers => Total},
    case push_blob(BaseUrl, Repo, Digest, Data, Token, LayerOpts) of
        ok ->
            push_layers_with_index(Rest, BaseUrl, Repo, Token, Opts, Index + 1, Total);
        {error, _} = Err ->
            Err
    end.

%% Push base image layers to target registry
-spec push_base_layers(ocibuild:image(), string(), binary(), binary(), push_opts()) ->
    ok | {error, term()}.
push_base_layers(Image, BaseUrl, Repo, Token, Opts) ->
    case maps:get(base_manifest, Image, undefined) of
        undefined ->
            %% No base image, nothing to do
            ok;
        BaseManifest ->
            BaseLayers = maps:get(~"layers", BaseManifest, []),
            BaseRef = maps:get(base, Image, none),
            BaseAuth = maps:get(auth, Image, #{}),
            push_base_layers_list(BaseLayers, BaseRef, BaseAuth, BaseUrl, Repo, Token, Opts)
    end.

-spec push_base_layers_list(list(), term(), map(), string(), binary(), binary(), push_opts()) ->
    ok | {error, term()}.
push_base_layers_list([], _BaseRef, _BaseAuth, _BaseUrl, _Repo, _Token, _Opts) ->
    ok;
push_base_layers_list([Layer | Rest], BaseRef, BaseAuth, BaseUrl, Repo, Token, Opts) ->
    Digest = maps:get(~"digest", Layer),

    %% Check if blob already exists in target registry
    CheckUrl = io_lib:format(
        "~s/v2/~s/blobs/~s",
        [BaseUrl, binary_to_list(Repo), binary_to_list(Digest)]
    ),
    Headers = auth_headers(Token),

    case ?MODULE:http_head(lists:flatten(CheckUrl), Headers) of
        {ok, _} ->
            %% Layer already exists in target, skip
            push_base_layers_list(Rest, BaseRef, BaseAuth, BaseUrl, Repo, Token, Opts);
        {error, _} ->
            %% Need to download from source and upload to target
            case download_and_upload_layer(BaseRef, BaseAuth, Digest, BaseUrl, Repo, Token, Opts) of
                ok ->
                    push_base_layers_list(Rest, BaseRef, BaseAuth, BaseUrl, Repo, Token, Opts);
                {error, _} = Err ->
                    Err
            end
    end.

-spec download_and_upload_layer(term(), map(), binary(), string(), binary(), binary(), push_opts()) ->
    ok | {error, term()}.
download_and_upload_layer(none, _BaseAuth, _Digest, _BaseUrl, _Repo, _Token, _Opts) ->
    {error, no_base_ref};
download_and_upload_layer(
    {SrcRegistry, SrcRepo, _SrcRef}, BaseAuth, Digest, BaseUrl, Repo, Token, Opts
) ->
    %% Download blob from source registry
    case pull_blob(SrcRegistry, SrcRepo, Digest, BaseAuth) of
        {ok, Data} ->
            %% Upload to target registry
            push_blob(BaseUrl, Repo, Digest, Data, Token, Opts);
        {error, _} = Err ->
            Err
    end.

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
    ChunkSize = maps:get(chunk_size, Opts, ?DEFAULT_CHUNK_SIZE),
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
    ChunkSize = maps:get(chunk_size, Opts, ?DEFAULT_CHUNK_SIZE),
    TotalSize = byte_size(Data),
    ProgressFn = maps:get(progress, Opts, undefined),
    LayerIndex = maps:get(layer_index, Opts, 0),
    TotalLayers = maps:get(total_layers, Opts, 1),

    %% Start upload session
    case start_upload_session(BaseUrl, Repo, Token) of
        {ok, Session} ->
            %% Upload chunks
            upload_chunks_loop(
                Session,
                Data,
                Digest,
                0,
                ChunkSize,
                TotalSize,
                Token,
                ProgressFn,
                LayerIndex,
                TotalLayers
            );
        {error, _} = Err ->
            Err
    end.

%% Internal: loop through chunks and upload each one
-spec upload_chunks_loop(
    upload_session(),
    binary(),
    binary(),
    non_neg_integer(),
    pos_integer(),
    non_neg_integer(),
    binary(),
    progress_callback() | undefined,
    non_neg_integer(),
    non_neg_integer()
) -> ok | {error, term()}.
upload_chunks_loop(
    Session,
    Data,
    Digest,
    Offset,
    ChunkSize,
    TotalSize,
    Token,
    ProgressFn,
    LayerIndex,
    TotalLayers
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
                    upload_chunks_loop(
                        NewSession,
                        Data,
                        Digest,
                        Offset + ChunkSize,
                        ChunkSize,
                        TotalSize,
                        Token,
                        ProgressFn,
                        LayerIndex,
                        TotalLayers
                    );
                {error, _} = Err ->
                    Err
            end
    end.

%% Push manifest
-spec push_manifest(
    ocibuild:image(),
    string(),
    binary(),
    binary(),
    binary(),
    binary(),
    non_neg_integer()
) ->
    ok | {error, term()}.
push_manifest(Image, BaseUrl, Repo, Tag, Token, ConfigDigest, ConfigSize) ->
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

    Url = io_lib:format(
        "~s/v2/~s/manifests/~s",
        [BaseUrl, binary_to_list(Repo), binary_to_list(Tag)]
    ),
    Headers =
        auth_headers(Token) ++ [{"Content-Type", "application/vnd.oci.image.manifest.v1+json"}],

    case ?MODULE:http_put(lists:flatten(Url), Headers, ManifestJson) of
        {ok, _} ->
            ok;
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

%% Ensure ssl is started and our stand_alone httpc is running
-spec ensure_started() -> ok.
ensure_started() ->
    %% SSL is needed for HTTPS
    case ssl:start() of
        ok -> ok;
        {error, {already_started, _}} -> ok
    end,
    %% Start httpc in stand_alone mode (not supervised by inets)
    %% This allows clean shutdown when we're done
    ProfileName = httpc_profile_name(?HTTPC_PROFILE),
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
                    {ok, Pid} = inets:start(httpc, [{profile, ?HTTPC_PROFILE}], stand_alone),
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

-doc "Stop the stand_alone httpc to allow clean VM exit.".
-spec stop_httpc() -> ok.
stop_httpc() ->
    case persistent_term:get(?HTTPC_KEY, undefined) of
        undefined ->
            ok;
        Pid ->
            %% Clear the persistent_term first so no new requests use this pid
            _ = persistent_term:erase(?HTTPC_KEY),
            %% Stop httpc in a separate process to avoid EXIT signal propagation
            %% The spawn will unregister the name and stop the httpc process
            CleanupPid = spawn(fun() ->
                _ = (catch unregister(httpc_profile_name(?HTTPC_PROFILE))),
                _ = inets:stop(stand_alone, Pid)
            end),
            %% Wait synchronously for cleanup process to finish, with a timeout
            Ref = erlang:monitor(process, CleanupPid),
            receive
                {'DOWN', Ref, process, CleanupPid, _Reason} ->
                    ok
            after ?DEFAULT_TIMEOUT ->
                %% Cleanup took too long; stop waiting but keep going
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
    ensure_started(),
    %% Add Connection: close to prevent stale connection reuse issues
    AllHeaders = Headers ++ [{"Connection", "close"}],
    Request = {Url, AllHeaders},
    %% Disable autoredirect - we handle redirects manually to strip auth headers
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}, {autoredirect, false}, {ssl, ssl_opts()}],
    Opts = [{body_format, binary}, {socket_opts, [{keepalive, false}]}],
    case httpc:request(get, Request, HttpOpts, Opts, ?HTTPC_PROFILE) of
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
    ensure_started(),
    AllHeaders = Headers ++ [{"Connection", "close"}],
    Request = {Url, AllHeaders},
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}, {ssl, ssl_opts()}],
    Opts = [{socket_opts, [{keepalive, false}]}],
    case httpc:request(head, Request, HttpOpts, Opts, ?HTTPC_PROFILE) of
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
    ensure_started(),
    ContentType = proplists:get_value("Content-Type", Headers, "application/octet-stream"),
    AllHeaders = Headers ++ [{"Connection", "close"}],
    Request = {Url, AllHeaders, ContentType, Body},
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}, {ssl, ssl_opts()}],
    Opts = [{body_format, binary}, {socket_opts, [{keepalive, false}]}],
    case httpc:request(post, Request, HttpOpts, Opts, ?HTTPC_PROFILE) of
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
    ensure_started(),
    ContentType = proplists:get_value("Content-Type", Headers, "application/octet-stream"),
    AllHeaders = Headers ++ [{"Connection", "close"}],
    Request = {Url, AllHeaders, ContentType, Body},
    HttpOpts = [{timeout, Timeout}, {ssl, ssl_opts()}],
    Opts = [{body_format, binary}, {socket_opts, [{keepalive, false}]}],
    case httpc:request(put, Request, HttpOpts, Opts, ?HTTPC_PROFILE) of
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
    ensure_started(),
    ContentType = proplists:get_value("Content-Type", Headers, "application/octet-stream"),
    AllHeaders = Headers ++ [{"Connection", "close"}],
    Request = {Url, AllHeaders, ContentType, Body},
    HttpOpts = [{timeout, Timeout}, {ssl, ssl_opts()}],
    Opts = [{body_format, binary}, {socket_opts, [{keepalive, false}]}],
    case httpc:request(patch, Request, HttpOpts, Opts, ?HTTPC_PROFILE) of
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
    non_neg_integer() | unknown
) ->
    {ok, binary()} | {error, term()}.
http_get_with_progress(Url, Headers, undefined, _Phase, _TotalBytes) ->
    %% No progress callback - use regular http_get
    ?MODULE:http_get(Url, Headers);
http_get_with_progress(Url, Headers, ProgressFn, Phase, TotalBytes) ->
    http_get_with_progress(Url, Headers, ProgressFn, Phase, TotalBytes, 5).

-spec http_get_with_progress(
    string(),
    [{string(), string()}],
    progress_callback(),
    progress_phase(),
    non_neg_integer() | unknown,
    non_neg_integer()
) ->
    {ok, binary()} | {error, term()}.
http_get_with_progress(_Url, _Headers, _ProgressFn, _Phase, _TotalBytes, 0) ->
    {error, too_many_redirects};
http_get_with_progress(Url, Headers, ProgressFn, Phase, TotalBytes, RedirectsLeft) ->
    ensure_started(),
    AllHeaders = Headers ++ [{"Connection", "close"}],
    Request = {Url, AllHeaders},
    %% Use streaming mode to receive chunks
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}, {autoredirect, false}, {ssl, ssl_opts()}],
    Opts = [
        {sync, false},
        {stream, self},
        {socket_opts, [{keepalive, false}]}
    ],

    case httpc:request(get, Request, HttpOpts, Opts, ?HTTPC_PROFILE) of
        {ok, RequestId} ->
            receive_stream(
                RequestId, ProgressFn, Phase, TotalBytes, <<>>, Url, Headers, RedirectsLeft
            );
        {error, Reason} ->
            {error, Reason}
    end.

%% Receive streaming response chunks
-spec receive_stream(
    reference(),
    progress_callback(),
    progress_phase(),
    non_neg_integer() | unknown,
    binary(),
    string(),
    [{string(), string()}],
    non_neg_integer()
) ->
    {ok, binary()} | {error, term()}.
receive_stream(RequestId, ProgressFn, Phase, TotalBytes, Acc, Url, Headers, RedirectsLeft) ->
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
            receive_stream(
                RequestId, ProgressFn, Phase, ActualTotal, Acc, Url, Headers, RedirectsLeft
            );
        {http, {RequestId, stream, BinBodyPart}} ->
            NewAcc = <<Acc/binary, BinBodyPart/binary>>,
            BytesReceived = byte_size(NewAcc),
            maybe_report_progress(ProgressFn, #{
                phase => Phase,
                bytes_received => BytesReceived,
                total_bytes => TotalBytes
            }),
            receive_stream(
                RequestId, ProgressFn, Phase, TotalBytes, NewAcc, Url, Headers, RedirectsLeft
            );
        {http, {RequestId, stream_end, _FinalHeaders}} ->
            %% Final progress report
            maybe_report_progress(ProgressFn, #{
                phase => Phase,
                bytes_received => byte_size(Acc),
                total_bytes => byte_size(Acc)
            }),
            {ok, Acc};
        {http, {RequestId, {{_, Status, _}, _RespHeaders, Body}}} when
            Status >= 200, Status < 300
        ->
            %% Non-streaming response (small body)
            maybe_report_progress(ProgressFn, #{
                phase => Phase,
                bytes_received => byte_size(Body),
                total_bytes => byte_size(Body)
            }),
            {ok, Body};
        {http, {RequestId, {{_, Status, _}, RespHeaders, _Body}}} when
            Status =:= 301; Status =:= 302; Status =:= 303; Status =:= 307; Status =:= 308
        ->
            %% Handle redirect
            case get_redirect_location(RespHeaders) of
                {ok, RedirectUrl} ->
                    RedirectHeaders = strip_auth_headers(Headers),
                    http_get_with_progress(
                        RedirectUrl,
                        RedirectHeaders,
                        ProgressFn,
                        Phase,
                        TotalBytes,
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

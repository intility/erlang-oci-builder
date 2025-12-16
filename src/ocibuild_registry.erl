%%%-------------------------------------------------------------------
-module(ocibuild_registry).
-moduledoc """
OCI Distribution registry client.

Implements the OCI Distribution Specification for pulling and pushing
container images to registries like Docker Hub, GHCR, etc.

See: https://github.com/opencontainers/distribution-spec
""".

-export([pull_manifest/3, pull_manifest/4, pull_blob/3, pull_blob/4, push/5,
         check_blob_exists/4]).

-define(DEFAULT_TIMEOUT, 30000).
%% Registry URL mappings
-define(REGISTRY_URLS,
        #{~"docker.io" => "https://registry-1.docker.io",
          ~"ghcr.io" => "https://ghcr.io",
          ~"gcr.io" => "https://gcr.io",
          ~"quay.io" => "https://quay.io"}).

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
    BaseUrl = registry_url(Registry),

    %% Get auth token if needed
    case get_auth_token(Registry, Repo, Auth) of
        {ok, Token} ->
            %% Fetch manifest (accept both single manifests and manifest lists)
            ManifestUrl =
                io_lib:format("~s/v2/~s/manifests/~s",
                              [BaseUrl, binary_to_list(Repo), binary_to_list(Ref)]),
            Headers =
                auth_headers(Token)
                ++ [{"Accept", "application/vnd.oci.image.index.v1+json"},
                    {"Accept", "application/vnd.docker.distribution.manifest.list.v2+json"},
                    {"Accept", "application/vnd.oci.image.manifest.v1+json"},
                    {"Accept", "application/vnd.docker.distribution.manifest.v2+json"}],

            case http_get(lists:flatten(ManifestUrl), Headers) of
                {ok, ManifestJson} ->
                    Manifest = ocibuild_json:decode(ManifestJson),
                    %% Check if this is a manifest list (multi-platform image)
                    case is_manifest_list(Manifest) of
                        true ->
                            %% Select platform-specific manifest and fetch it
                            case select_platform_manifest(Manifest) of
                                {ok, PlatformDigest} ->
                                    pull_manifest(Registry, Repo, PlatformDigest, Auth);
                                {error, _} = Err ->
                                    Err
                            end;
                        false ->
                            %% Single manifest - fetch config blob
                            ConfigDescriptor = maps:get(~"config", Manifest),
                            ConfigDigest = maps:get(~"digest", ConfigDescriptor),
                            case pull_blob(Registry, Repo, ConfigDigest, Auth) of
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
    BaseUrl = registry_url(Registry),

    case get_auth_token(Registry, Repo, Auth) of
        {ok, Token} ->
            Url = io_lib:format("~s/v2/~s/blobs/~s",
                                [BaseUrl, binary_to_list(Repo), binary_to_list(Digest)]),
            Headers = auth_headers(Token),
            http_get(lists:flatten(Url), Headers);
        {error, _} = Err ->
            Err
    end.

-doc "Check if a blob exists in the registry.".
-spec check_blob_exists(binary(), binary(), binary(), map()) -> boolean().
check_blob_exists(Registry, Repo, Digest, Auth) ->
    BaseUrl = registry_url(Registry),

    case get_auth_token(Registry, Repo, Auth) of
        {ok, Token} ->
            Url = io_lib:format("~s/v2/~s/blobs/~s",
                                [BaseUrl, binary_to_list(Repo), binary_to_list(Digest)]),
            Headers = auth_headers(Token),
            case http_head(lists:flatten(Url), Headers) of
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
    BaseUrl = registry_url(Registry),

    case get_auth_token(Registry, Repo, Auth) of
        {ok, Token} ->
            %% Push layers
            case push_layers(Image, BaseUrl, Repo, Token) of
                ok ->
                    %% Push config
                    case push_config(Image, BaseUrl, Repo, Token) of
                        {ok, ConfigDigest, ConfigSize} ->
                            %% Push manifest
                            push_manifest(Image,
                                          BaseUrl,
                                          Repo,
                                          Tag,
                                          Token,
                                          ConfigDigest,
                                          ConfigSize);
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
        _error ->
            "https://" ++ binary_to_list(Registry)
    end.

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
    CandidateManifests = [M || M <- Manifests,
                               not is_attestation_manifest(M)],

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
find_platform_manifest([#{~"platform" := Platform, ~"digest" := Digest} | Rest],
                       TargetOs, TargetArch) ->
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
    Os = ~"linux",  % Container images are always for Linux
    Arch = case erlang:system_info(system_architecture) of
               Arch0 when is_list(Arch0) ->
                   normalize_arch(list_to_binary(Arch0));
               _ ->
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
        _ -> ~"amd64"  % Default fallback
    end.

%% Get authentication token
-spec get_auth_token(binary(), binary(), map()) ->
                        {ok, binary() | none} | {error, term()}.
get_auth_token(~"docker.io", Repo, Auth) ->
    %% Docker Hub uses token authentication
    docker_hub_auth(Repo, Auth);
get_auth_token(_Registry, _Repo, #{token := Token}) ->
    {ok, Token};
get_auth_token(_Registry, _Repo, #{username := User, password := Pass}) ->
    %% Basic auth - encode as base64
    Encoded = base64:encode(<<User/binary, ":", Pass/binary>>), %% Keep interpolation
    {ok, {basic, Encoded}};
get_auth_token(_Registry, _Repo, #{}) ->
    {ok, none}.

%% Docker Hub specific authentication
-spec docker_hub_auth(binary(), map()) -> {ok, binary()} | {error, term()}.
docker_hub_auth(Repo, Auth) ->
    %% Docker Hub requires getting a token from auth.docker.io
    Scope = "repository:" ++ binary_to_list(Repo) ++ ":pull,push",
    Url = "https://auth.docker.io/token?service=registry.docker.io&scope="
          ++ uri_string:quote(Scope),

    Headers =
        case Auth of
            #{username := User, password := Pass} ->
                Encoded = base64:encode(<<User/binary, ":", Pass/binary>>),
                [{"Authorization", "Basic " ++ binary_to_list(Encoded)}];
            _ ->
                []
        end,

    case http_get(Url, Headers) of
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

%% Build auth headers
-spec auth_headers(binary() | {basic, binary()} | none) -> [{string(), string()}].
auth_headers(none) ->
    [];
auth_headers({basic, Encoded}) ->
    [{"Authorization", "Basic " ++ binary_to_list(Encoded)}];
auth_headers(Token) when is_binary(Token) ->
    [{"Authorization", "Bearer " ++ binary_to_list(Token)}].

%% Push all layers
-spec push_layers(ocibuild:image(), string(), binary(), binary()) -> ok | {error, term()}.
push_layers(#{layers := Layers}, BaseUrl, Repo, Token) ->
    lists:foldl(fun (#{digest := Digest, data := Data}, ok) ->
                        push_blob(BaseUrl, Repo, Digest, Data, Token);
                    (_, {error, _} = Err) ->
                        Err
                end,
                ok,
                Layers).

%% Push config and return its digest and size
-spec push_config(ocibuild:image(), string(), binary(), binary()) ->
                     {ok, binary(), non_neg_integer()} | {error, term()}.
push_config(#{config := Config}, BaseUrl, Repo, Token) ->
    ConfigJson = ocibuild_json:encode(Config),
    Digest = ocibuild_digest:sha256(ConfigJson),
    case push_blob(BaseUrl, Repo, Digest, ConfigJson, Token) of
        ok ->
            {ok, Digest, byte_size(ConfigJson)};
        {error, _} = Err ->
            Err
    end.

%% Push a single blob
-spec push_blob(string(), binary(), binary(), binary(), binary()) -> ok | {error, term()}.
push_blob(BaseUrl, Repo, Digest, Data, Token) ->
    %% Check if blob already exists
    CheckUrl =
        io_lib:format("~s/v2/~s/blobs/~s",
                      [BaseUrl, binary_to_list(Repo), binary_to_list(Digest)]),
    Headers = auth_headers(Token),

    case http_head(lists:flatten(CheckUrl), Headers) of
        {ok, _} ->
            %% Blob already exists
            ok;
        {error, _} ->
            %% Need to upload
            do_push_blob(BaseUrl, Repo, Digest, Data, Token)
    end.

%% Actually upload a blob
-spec do_push_blob(string(), binary(), binary(), binary(), binary()) ->
                      ok | {error, term()}.
do_push_blob(BaseUrl, Repo, Digest, Data, Token) ->
    %% Start upload session
    InitUrl = io_lib:format("~s/v2/~s/blobs/uploads/", [BaseUrl, binary_to_list(Repo)]),
    Headers = auth_headers(Token),

    case http_post(lists:flatten(InitUrl), Headers, <<>>) of
        {ok, _, ResponseHeaders} ->
            %% Get upload location
            case proplists:get_value("location", ResponseHeaders) of
                undefined ->
                    {error, no_upload_location};
                Location ->
                    %% Complete upload with PUT
                    PutUrl = Location ++ "&digest=" ++ binary_to_list(Digest),
                    PutHeaders =
                        Headers
                        ++ [{"Content-Type", "application/octet-stream"},
                            {"Content-Length", integer_to_list(byte_size(Data))}],
                    case http_put(PutUrl, PutHeaders, Data) of
                        {ok, _} ->
                            ok;
                        {error, _} = Err ->
                            Err
                    end
            end;
        {error, _} = Err ->
            Err
    end.

%% Push manifest
-spec push_manifest(ocibuild:image(),
                    string(),
                    binary(),
                    binary(),
                    binary(),
                    binary(),
                    non_neg_integer()) ->
                       ok | {error, term()}.
push_manifest(Image, BaseUrl, Repo, Tag, Token, ConfigDigest, ConfigSize) ->
    LayerDescriptors =
        [#{~"mediaType" => MediaType,
           ~"digest" => Digest,
           ~"size" => Size}
         || #{media_type := MediaType,
              digest := Digest,
              size := Size}
                <- maps:get(layers, Image, [])],

    {ManifestJson, _} =
        ocibuild_manifest:build(#{~"mediaType" =>
                                      ~"application/vnd.oci.image.config.v1+json",
                                  ~"digest" => ConfigDigest,
                                  ~"size" => ConfigSize},
                                LayerDescriptors),

    Url = io_lib:format("~s/v2/~s/manifests/~s",
                        [BaseUrl, binary_to_list(Repo), binary_to_list(Tag)]),
    Headers =
        auth_headers(Token) ++ [{"Content-Type", "application/vnd.oci.image.manifest.v1+json"}],

    case http_put(lists:flatten(Url), Headers, ManifestJson) of
        {ok, _} ->
            ok;
        {error, _} = Err ->
            Err
    end.

%%%===================================================================
%%% HTTP helpers (using httpc)
%%%===================================================================

%% Ensure inets is started
-spec ensure_started() -> ok.
ensure_started() ->
    case inets:start() of
        ok ->
            ok;
        {error, {already_started, _}} ->
            ok
    end,
    case ssl:start() of
        ok ->
            ok;
        {error, {already_started, _}} ->
            ok
    end,
    ok.

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
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}, {autoredirect, false}],
    Opts = [{body_format, binary}, {socket_opts, [{keepalive, false}]}],
    case httpc:request(get, Request, HttpOpts, Opts) of
        {ok, {{_, Status, _}, _, Body}} when Status >= 200, Status < 300 ->
            {ok, Body};
        {ok, {{_, Status, _}, RespHeaders, _}} when Status =:= 301;
                                                     Status =:= 302;
                                                     Status =:= 303;
                                                     Status =:= 307;
                                                     Status =:= 308 ->
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
    [{K, V} || {K, V} <- Headers,
               string:lowercase(K) =/= "authorization"].

-spec http_head(string(), [{string(), string()}]) ->
                   {ok, [{string(), string()}]} | {error, term()}.
http_head(Url, Headers) ->
    ensure_started(),
    AllHeaders = Headers ++ [{"Connection", "close"}],
    Request = {Url, AllHeaders},
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}],
    Opts = [{socket_opts, [{keepalive, false}]}],
    case httpc:request(head, Request, HttpOpts, Opts) of
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
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}],
    Opts = [{body_format, binary}, {socket_opts, [{keepalive, false}]}],
    case httpc:request(post, Request, HttpOpts, Opts) of
        {ok, {{_, Status, _}, ResponseHeaders, ResponseBody}} when Status >= 200, Status < 300 ->
            {ok, ResponseBody, normalize_headers(ResponseHeaders)};
        {ok, {{_, Status, Reason}, _, _}} ->
            {error, {http_error, Status, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec http_put(string(), [{string(), string()}], binary()) ->
                  {ok, binary()} | {error, term()}.
http_put(Url, Headers, Body) ->
    ensure_started(),
    ContentType = proplists:get_value("Content-Type", Headers, "application/octet-stream"),
    AllHeaders = Headers ++ [{"Connection", "close"}],
    Request = {Url, AllHeaders, ContentType, Body},
    HttpOpts = [{timeout, ?DEFAULT_TIMEOUT}],
    Opts = [{body_format, binary}, {socket_opts, [{keepalive, false}]}],
    case httpc:request(put, Request, HttpOpts, Opts) of
        {ok, {{_, Status, _}, _, ResponseBody}} when Status >= 200, Status < 300 ->
            {ok, ResponseBody};
        {ok, {{_, Status, Reason}, _, _}} ->
            {error, {http_error, Status, Reason}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Normalize headers to lowercase keys
-spec normalize_headers([{string(), string()}]) -> [{string(), string()}].
normalize_headers(Headers) ->
    [{string:lowercase(K), V} || {K, V} <- Headers].

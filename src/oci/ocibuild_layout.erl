%%%-------------------------------------------------------------------
-module(ocibuild_layout).
-moduledoc """
OCI image layout handling.

Produces OCI Image Layout format for use with `podman load`,
`skopeo`, or direct filesystem storage.

The layout structure is:
```
myimage/
├── oci-layout           # {"imageLayoutVersion": "1.0.0"}
├── index.json           # Entry point
└── blobs/
    └── sha256/
        ├── <manifest>   # Manifest JSON
        ├── <config>     # Config JSON
        └── <layers...>  # Layer tarballs (gzip compressed)
```

See: https://github.com/opencontainers/image-spec/blob/main/image-layout.md
""".

-export([export_directory/2, save_tarball/2, save_tarball/3]).

%% Parallel utilities (exported for potential reuse)
-export([pmap_bounded/3]).

%% Manifest building utilities (used by ocibuild_release for referrer push)
-export([build_config_blob/1, build_layer_descriptors/1]).

%% Exports for testing
-ifdef(TEST).
-export([blob_path/1, build_index/3]).
-endif.

%% Default max concurrent downloads/uploads
-define(DEFAULT_MAX_CONCURRENCY, 4).

%% Common file permission modes

% rw-r--r-- (regular files)
-define(MODE_FILE, 8#644).

-doc """
Export image as an OCI layout directory.

Creates the standard OCI directory structure at the given path.
```
ok = ocibuild_layout:export_directory(Image, "./myimage").
```
""".
-spec export_directory(ocibuild:image(), file:filename()) -> ok | {error, term()}.
export_directory(Image, Path) ->
    try
        %% Create directory structure
        BlobsDir = filename:join([Path, "blobs", "sha256"]),
        ok =
            filelib:ensure_dir(
                filename:join(BlobsDir, "dummy")
            ),

        %% Build all the components
        {ConfigJson, ConfigDigest} = build_config_blob(Image),
        LayerDescriptors = build_layer_descriptors(Image),
        Annotations = maps:get(annotations, Image, #{}),
        {ManifestJson, ManifestDigest} =
            ocibuild_manifest:build(
                #{
                    ~"mediaType" =>
                        ~"application/vnd.oci.image.config.v1+json",
                    ~"digest" => ConfigDigest,
                    ~"size" => byte_size(ConfigJson)
                },
                LayerDescriptors,
                Annotations
            ),

        %% Write oci-layout
        OciLayout = ocibuild_json:encode(#{~"imageLayoutVersion" => ~"1.0.0"}),
        ok =
            file:write_file(
                filename:join(Path, "oci-layout"), OciLayout
            ),

        %% Write index.json
        Index = build_index(ManifestDigest, byte_size(ManifestJson), ~"latest"),
        IndexJson = ocibuild_json:encode(Index),
        ok =
            file:write_file(
                filename:join(Path, "index.json"), IndexJson
            ),

        %% Write config blob
        ConfigPath = filename:join(BlobsDir, ocibuild_digest:encoded(ConfigDigest)),
        ok = file:write_file(ConfigPath, ConfigJson),

        %% Write manifest blob
        ManifestPath = filename:join(BlobsDir, ocibuild_digest:encoded(ManifestDigest)),
        ok = file:write_file(ManifestPath, ManifestJson),

        %% Write layer blobs
        %% Layers are stored in reverse order, reverse for correct export order
        lists:foreach(
            fun(#{digest := Digest, data := Data}) ->
                LayerPath = filename:join(BlobsDir, ocibuild_digest:encoded(Digest)),
                ok = file:write_file(LayerPath, Data)
            end,
            lists:reverse(maps:get(layers, Image, []))
        ),

        ok
    catch
        throw:Reason ->
            {error, Reason};
        error:Reason:Stacktrace ->
            {error, {Reason, Stacktrace}};
        exit:Reason ->
            {error, {exit, Reason}}
    end.

-doc """
Save image as an OCI layout tarball.

Creates a tar.gz file that can be loaded with `podman load` or other OCI-compliant tools.
```
ok = ocibuild_layout:save_tarball(Image, "./myimage.tar.gz").
ok = ocibuild_layout:save_tarball(Image, "./myimage.tar.gz", #{tag => ~"myapp:1.0"}).
```

Options:
- `tag`: Image tag annotation (e.g., `~"myapp:1.0"`)

Note: OCI layout works with podman, skopeo, crane, buildah, and other OCI tools.
""".
-spec save_tarball(ocibuild:image(), file:filename()) -> ok | {error, term()}.
save_tarball(Image, Path) ->
    save_tarball(Image, Path, #{}).

-spec save_tarball(ocibuild:image() | [ocibuild:image()], file:filename(), map()) ->
    ok | {error, term()}.
save_tarball(Images, Path, Opts) when is_list(Images), length(Images) > 1 ->
    %% Multi-platform: save all images with an image index
    save_tarball_multi(Images, Path, Opts);
save_tarball([Image], Path, Opts) ->
    %% Single image in list - treat as regular save
    save_tarball(Image, Path, Opts);
save_tarball(Image, Path, Opts) when is_map(Image) ->
    try
        Tag = maps:get(tag, Opts, ~"latest"),

        %% Build all the blobs and metadata
        {ConfigJson, ConfigDigest} = build_config_blob(Image),
        LayerDescriptors = build_layer_descriptors(Image),
        Annotations = maps:get(annotations, Image, #{}),
        {ManifestJson, ManifestDigest} =
            ocibuild_manifest:build(
                #{
                    ~"mediaType" =>
                        ~"application/vnd.oci.image.config.v1+json",
                    ~"digest" => ConfigDigest,
                    ~"size" => byte_size(ConfigJson)
                },
                LayerDescriptors,
                Annotations
            ),

        %% Build oci-layout
        OciLayout = ocibuild_json:encode(#{~"imageLayoutVersion" => ~"1.0.0"}),

        %% Build index.json with tag annotation for podman/skopeo
        Index = build_index(ManifestDigest, byte_size(ManifestJson), Tag),
        IndexJson = ocibuild_json:encode(Index),

        %% Collect all files for the tarball
        %% Include base image layers if available
        BaseLayerFiles = build_base_layers(Image),

        Files =
            [
                {~"oci-layout", OciLayout, ?MODE_FILE},
                {~"index.json", IndexJson, ?MODE_FILE},
                {blob_path(ConfigDigest), ConfigJson, ?MODE_FILE},
                {blob_path(ManifestDigest), ManifestJson, ?MODE_FILE}
            ] ++
                %% Layers are stored in reverse order, reverse for correct export order
                [
                    {blob_path(Digest), Data, ?MODE_FILE}
                 || #{digest := Digest, data := Data} <- lists:reverse(maps:get(layers, Image, []))
                ] ++
                BaseLayerFiles,

        %% Create the tarball
        TarData = ocibuild_tar:create(Files),
        Compressed = zlib:gzip(TarData),

        ok = file:write_file(Path, Compressed)
    catch
        error:Reason ->
            {error, Reason}
    end.

%% Save multiple platform images as a multi-platform tarball
-spec save_tarball_multi([ocibuild:image()], file:filename(), map()) -> ok | {error, term()}.
save_tarball_multi(Images, Path, Opts) ->
    try
        Tag = maps:get(tag, Opts, ~"latest"),

        %% Build oci-layout
        OciLayout = ocibuild_json:encode(#{~"imageLayoutVersion" => ~"1.0.0"}),

        %% Build each platform's manifest and collect blobs
        {ManifestDescriptors, AllFiles} = build_platform_manifests(Images),

        %% Build the image index
        Index = build_multi_platform_index(ManifestDescriptors, Tag),
        IndexJson = ocibuild_json:encode(Index),

        %% Combine all files
        Files =
            [
                {~"oci-layout", OciLayout, ?MODE_FILE},
                {~"index.json", IndexJson, ?MODE_FILE}
            ] ++ AllFiles,

        %% Create the tarball
        TarData = ocibuild_tar:create(Files),
        Compressed = zlib:gzip(TarData),

        ok = file:write_file(Path, Compressed)
    catch
        error:Reason ->
            {error, Reason}
    end.

%% Build manifests for each platform and collect all blob files
%% Processes platforms in parallel for better performance
-spec build_platform_manifests([ocibuild:image()]) ->
    {[{ocibuild:platform(), binary(), non_neg_integer()}], [{binary(), binary(), integer()}]}.
build_platform_manifests([]) ->
    {[], []};
build_platform_manifests(Images) ->
    %% Process all platforms in parallel
    Results = pmap_bounded(
        fun(Image) -> build_single_platform(Image) end,
        Images,
        ?DEFAULT_MAX_CONCURRENCY
    ),

    %% Collect results maintaining platform order
    {ManifestDescs, FilesList} = lists:unzip(Results),
    AllFiles = lists:flatten(FilesList),

    %% Deduplicate files by path (layers may be shared between platforms)
    UniqueFiles = deduplicate_files(AllFiles),
    {ManifestDescs, UniqueFiles}.

%% Build manifest and collect files for a single platform image
-spec build_single_platform(ocibuild:image()) ->
    {{ocibuild:platform(), binary(), non_neg_integer()}, [{binary(), binary(), integer()}]}.
build_single_platform(Image) ->
    Platform = maps:get(platform, Image, #{os => ~"linux", architecture => ~"amd64"}),

    %% Build config blob
    {ConfigJson, ConfigDigest} = build_config_blob(Image),

    %% Build layer descriptors
    LayerDescriptors = build_layer_descriptors(Image),
    Annotations = maps:get(annotations, Image, #{}),

    %% Build manifest
    {ManifestJson, ManifestDigest} =
        ocibuild_manifest:build(
            #{
                ~"mediaType" => ~"application/vnd.oci.image.config.v1+json",
                ~"digest" => ConfigDigest,
                ~"size" => byte_size(ConfigJson)
            },
            LayerDescriptors,
            Annotations
        ),

    ManifestDesc = {Platform, ManifestDigest, byte_size(ManifestJson)},

    %% Collect files for this platform (downloads layers in parallel)
    BaseLayerFiles = build_base_layers(Image),
    PlatformFiles =
        [
            {blob_path(ConfigDigest), ConfigJson, ?MODE_FILE},
            {blob_path(ManifestDigest), ManifestJson, ?MODE_FILE}
        ] ++
            [
                {blob_path(Digest), Data, ?MODE_FILE}
             || #{digest := Digest, data := Data} <- lists:reverse(maps:get(layers, Image, []))
            ] ++
            BaseLayerFiles,

    {ManifestDesc, PlatformFiles}.

%% Deduplicate files by path (first occurrence wins)
-spec deduplicate_files([{binary(), binary(), integer()}]) -> [{binary(), binary(), integer()}].
deduplicate_files(Files) ->
    {_, Unique} = lists:foldl(
        fun({Path, _, _} = File, {Seen, Acc}) ->
            case sets:is_element(Path, Seen) of
                true -> {Seen, Acc};
                false -> {sets:add_element(Path, Seen), [File | Acc]}
            end
        end,
        {sets:new(), []},
        Files
    ),
    lists:reverse(Unique).

%% Build a multi-platform image index
-spec build_multi_platform_index([{ocibuild:platform(), binary(), non_neg_integer()}], binary()) ->
    map().
build_multi_platform_index(ManifestDescriptors, Tag) ->
    Manifests = [
        #{
            ~"mediaType" => ~"application/vnd.oci.image.manifest.v1+json",
            ~"digest" => Digest,
            ~"size" => Size,
            ~"platform" => build_platform_json(Platform)
        }
     || {Platform, Digest, Size} <- ManifestDescriptors
    ],
    #{
        ~"schemaVersion" => 2,
        ~"mediaType" => ~"application/vnd.oci.image.index.v1+json",
        ~"manifests" => Manifests,
        ~"annotations" => #{
            ~"org.opencontainers.image.ref.name" => Tag
        }
    }.

%% Build platform JSON for image index
-spec build_platform_json(ocibuild:platform()) -> map().
build_platform_json(Platform) ->
    Base = #{
        ~"os" => maps:get(os, Platform),
        ~"architecture" => maps:get(architecture, Platform)
    },
    case maps:find(variant, Platform) of
        {ok, Variant} -> Base#{~"variant" => Variant};
        error -> Base
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Download base image layers from registry (with caching)
%% Uses parallel downloads with bounded concurrency for better performance
-spec build_base_layers(ocibuild:image()) -> [{binary(), binary(), integer()}].
build_base_layers(
    #{
        base := {Registry, Repo, _Tag},
        base_manifest := #{~"layers" := ManifestLayers}
    } = Image
) ->
    Auth = maps:get(auth, Image, #{}),
    Platform = maps:get(platform, Image, undefined),
    TotalLayers = length(ManifestLayers),

    %% Create indexed work items for parallel download
    IndexedLayers = lists:zip(lists:seq(1, TotalLayers), ManifestLayers),

    %% Download function for each layer
    DownloadFn = fun({Index, #{~"digest" := Digest, ~"size" := Size}}) ->
        CompressedData = get_or_download_layer(
            Registry, Repo, Digest, Auth, Size, Index, TotalLayers, Platform
        ),
        {blob_path(Digest), CompressedData, ?MODE_FILE}
    end,

    %% Download layers in parallel with supervised HTTP workers
    %% Each worker gets its own httpc profile for isolation
    ocibuild_http:pmap(DownloadFn, IndexedLayers, ?DEFAULT_MAX_CONCURRENCY);
build_base_layers(_Image) ->
    [].

%% Get layer from cache or download from registry
-spec get_or_download_layer(
    binary(),
    binary(),
    binary(),
    map(),
    non_neg_integer(),
    pos_integer(),
    pos_integer(),
    ocibuild:platform() | undefined
) -> binary().
get_or_download_layer(Registry, Repo, Digest, Auth, Size, Index, TotalLayers, Platform) ->
    case ocibuild_cache:get(Digest) of
        {ok, CachedData} ->
            io:format("  Layer ~B/~B cached (~s)~n", [
                Index, TotalLayers, ocibuild_release:format_bytes(Size)
            ]),
            CachedData;
        {error, _} ->
            %% Not in cache or corrupted, download from registry
            %% Register a progress bar for this download
            Label = make_layer_label(Index, TotalLayers, Platform),
            ProgressRef = ocibuild_progress:register_bar(#{
                label => Label,
                total => Size
            }),

            case pull_blob_with_progress(Registry, Repo, Digest, Auth, ProgressRef, 3) of
                {ok, CompressedData} ->
                    ocibuild_progress:complete(ProgressRef),
                    %% Cache the downloaded layer for future builds
                    case ocibuild_cache:put(Digest, CompressedData) of
                        ok ->
                            ok;
                        {error, CacheErr} ->
                            %% Log but don't fail - caching is best-effort
                            logger:warning("Failed to cache layer ~s: ~p", [Digest, CacheErr])
                    end,
                    CompressedData;
                {error, Reason} ->
                    %% Fail build instead of silently producing broken images
                    error({layer_download_failed, Digest, Reason})
            end
    end.

%% Create a label for a layer download
make_layer_label(Index, TotalLayers, undefined) ->
    iolist_to_binary(io_lib:format("Layer ~B/~B", [Index, TotalLayers]));
make_layer_label(Index, TotalLayers, #{architecture := Arch}) ->
    iolist_to_binary(io_lib:format("Layer ~B/~B (~s)", [Index, TotalLayers, Arch]));
make_layer_label(Index, TotalLayers, _Platform) ->
    iolist_to_binary(io_lib:format("Layer ~B/~B", [Index, TotalLayers])).

%% Pull blob with progress reporting and retry logic
-spec pull_blob_with_progress(
    binary(),
    binary(),
    binary(),
    map(),
    reference(),
    non_neg_integer()
) ->
    {ok, binary()} | {error, term()}.
pull_blob_with_progress(Registry, Repo, Digest, Auth, ProgressRef, MaxRetries) ->
    %% Create a progress callback that updates our progress bar
    ProgressFn = fun(Info) ->
        Bytes = maps:get(bytes_received, Info, 0),
        ocibuild_progress:update(ProgressRef, Bytes)
    end,
    Opts = #{progress => ProgressFn},
    ocibuild_registry:with_retry(
        fun() -> ocibuild_registry:pull_blob(Registry, Repo, Digest, Auth, Opts) end,
        MaxRetries
    ).

%% Build the config JSON blob
-spec build_config_blob(ocibuild:image()) -> {binary(), binary()}.
build_config_blob(#{config := Config}) ->
    %% Reverse diff_ids and history which are stored in reverse order for O(1) append
    Rootfs = maps:get(~"rootfs", Config),
    DiffIds = maps:get(~"diff_ids", Rootfs, []),
    History = maps:get(~"history", Config, []),
    ExportConfig = Config#{
        ~"rootfs" => Rootfs#{~"diff_ids" => lists:reverse(DiffIds)},
        ~"history" => lists:reverse(History)
    },
    Json = ocibuild_json:encode(ExportConfig),
    Digest = ocibuild_digest:sha256(Json),
    {Json, Digest}.

%% Build layer descriptors for the manifest
-spec build_layer_descriptors(ocibuild:image()) -> [map()].
build_layer_descriptors(#{base_manifest := BaseManifest, layers := NewLayers}) ->
    %% Include base image layers + new layers
    %% NewLayers are stored in reverse order, reverse for correct manifest order
    BaseLayers = maps:get(~"layers", BaseManifest, []),
    NewDescriptors =
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
                lists:reverse(NewLayers)
        ],
    BaseLayers ++ NewDescriptors;
build_layer_descriptors(#{layers := NewLayers}) ->
    %% No base image, just new layers (reverse for correct order)
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
            lists:reverse(NewLayers)
    ].

%% Build the index.json structure with tag annotation
-spec build_index(binary(), non_neg_integer(), binary()) -> map().
build_index(ManifestDigest, ManifestSize, Tag) ->
    #{
        ~"schemaVersion" => 2,
        ~"manifests" =>
            [
                #{
                    ~"mediaType" => ~"application/vnd.oci.image.manifest.v1+json",
                    ~"digest" => ManifestDigest,
                    ~"size" => ManifestSize,
                    ~"annotations" => #{
                        %% This annotation tells podman/skopeo what to name the image
                        ~"org.opencontainers.image.ref.name" => Tag
                    }
                }
            ]
    }.

%% Convert digest to blob path
-spec blob_path(binary()) -> binary().
blob_path(Digest) ->
    Encoded = ocibuild_digest:encoded(Digest),
    <<"blobs/sha256/", Encoded/binary>>.

%%%===================================================================
%%% Parallel utilities
%%%===================================================================

-doc """
Parallel map with bounded concurrency.

Executes `Fun` on each element of `List` in parallel, with at most
`MaxWorkers` concurrent executions. Results are returned in the same
order as the input list.

Example:
```
Results = pmap_bounded(fun(X) -> X * 2 end, [1, 2, 3, 4, 5], 2).
%% Returns [2, 4, 6, 8, 10] with max 2 concurrent workers
```

Throws if any worker fails with the original error.
""".
-spec pmap_bounded(fun((A) -> B), [A], pos_integer()) -> [B] when A :: term(), B :: term().
pmap_bounded(_Fun, [], _MaxWorkers) ->
    [];
pmap_bounded(Fun, List, MaxWorkers) ->
    Self = self(),
    Ref = make_ref(),
    Total = length(List),

    %% Create indexed work items: [{1, Item1}, {2, Item2}, ...]
    IndexedItems = lists:zip(lists:seq(1, Total), List),

    %% Run parallel execution with bounded concurrency
    ResultMap = pmap_run(Fun, IndexedItems, MaxWorkers, Self, Ref, 0, #{}),

    %% Extract results in original order
    [maps:get(I, ResultMap) || I <- lists:seq(1, Total)].

%% Internal: run parallel execution loop
-spec pmap_run(
    fun((A) -> B),
    [{pos_integer(), A}],
    pos_integer(),
    pid(),
    reference(),
    non_neg_integer(),
    #{pos_integer() => B}
) -> #{pos_integer() => B} when A :: term(), B :: term().
pmap_run(_Fun, [], _MaxWorkers, _Self, _Ref, 0, Results) ->
    %% No pending items, no active workers -> done
    Results;
pmap_run(Fun, [], MaxWorkers, Self, Ref, ActiveCount, Results) when ActiveCount > 0 ->
    %% No more items to start, wait for active workers to complete
    receive
        {Ref, Index, {ok, Value}} ->
            pmap_run(Fun, [], MaxWorkers, Self, Ref, ActiveCount - 1, Results#{Index => Value});
        {Ref, _Index, {error, Class, Reason, Stack}} ->
            erlang:raise(Class, Reason, Stack)
    end;
pmap_run(Fun, Pending, MaxWorkers, Self, Ref, ActiveCount, Results) when
    ActiveCount >= MaxWorkers
->
    %% At max concurrency, wait for one to complete before starting more
    receive
        {Ref, Index, {ok, Value}} ->
            pmap_run(Fun, Pending, MaxWorkers, Self, Ref, ActiveCount - 1, Results#{Index => Value});
        {Ref, _Index, {error, Class, Reason, Stack}} ->
            erlang:raise(Class, Reason, Stack)
    end;
pmap_run(Fun, [{Index, Item} | Rest], MaxWorkers, Self, Ref, ActiveCount, Results) ->
    %% Spawn a new worker
    spawn_link(fun() ->
        try
            Value = Fun(Item),
            Self ! {Ref, Index, {ok, Value}}
        catch
            Class:Reason:Stack ->
                Self ! {Ref, Index, {error, Class, Reason, Stack}}
        end
    end),
    pmap_run(Fun, Rest, MaxWorkers, Self, Ref, ActiveCount + 1, Results).

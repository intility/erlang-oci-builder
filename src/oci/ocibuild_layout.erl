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

%% Load tarball for push (without rebuilding)
-export([load_tarball_for_push/1, load_tarball_for_push/2]).

%% Parallel utilities (exported for potential reuse)
-export([pmap_bounded/3]).

%% Manifest building utilities (used by ocibuild_release for referrer push)
-export([build_config_blob/1, build_layer_descriptors/1]).

%% Exports for testing
-ifdef(TEST).
-export([blob_path/1, build_index/3, extract_tarball/1, parse_index/1]).
-endif.

%% Default max concurrent downloads/uploads
-define(DEFAULT_MAX_CONCURRENCY, 4).

%% Memory threshold for hybrid load (100MB)
-define(DEFAULT_MEMORY_THRESHOLD, 100 * 1024 * 1024).

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

%%%===================================================================
%%% Load tarball for push
%%%===================================================================

-doc """
Load an OCI image tarball for pushing to a registry.

Parses the OCI Image Layout tarball and extracts all necessary data
for pushing: manifests, configs, and layer blobs.

Supports both single-image and multi-platform tarballs.
The tag is extracted from index.json annotations if present.

Uses hybrid memory/disk loading: small images (<100MB) are loaded
into memory, larger images are extracted to a temp directory.

```
{ok, Result} = ocibuild_layout:load_tarball_for_push("myimage.tar.gz"),
%% Result = #{
%%     images := [loaded_image()],
%%     tag := binary() | undefined,
%%     is_multi_platform := boolean(),
%%     cleanup := fun(() -> ok)
%% }
```
""".
-spec load_tarball_for_push(file:filename()) ->
    {ok, #{
        images := [loaded_image()],
        tag := binary() | undefined,
        is_multi_platform := boolean(),
        cleanup := fun(() -> ok)
    }}
    | {error, term()}.
load_tarball_for_push(Path) ->
    load_tarball_for_push(Path, #{}).

-doc """
Load an OCI image tarball with options.

Options:
- `memory_threshold`: File size threshold for in-memory loading (default: 100MB).
  Files smaller than this are loaded entirely into memory for speed.
  Larger files are extracted to a temp directory for memory efficiency.
""".
-spec load_tarball_for_push(file:filename(), #{memory_threshold => pos_integer()}) ->
    {ok, #{
        images := [loaded_image()],
        tag := binary() | undefined,
        is_multi_platform := boolean(),
        cleanup := fun(() -> ok)
    }}
    | {error, term()}.
load_tarball_for_push(Path, Opts) ->
    MemoryThreshold = maps:get(memory_threshold, Opts, ?DEFAULT_MEMORY_THRESHOLD),
    case filelib:file_size(Path) of
        0 ->
            {error, {tarball_not_found, Path}};
        FileSize when FileSize < MemoryThreshold ->
            %% Small image: extract to memory (fast)
            load_tarball_to_memory(Path);
        _ ->
            %% Large image: extract to temp directory (memory-efficient)
            load_tarball_to_disk(Path)
    end.

%% Type for loaded images (ready for push)
-type loaded_image() :: #{
    manifest := binary(),
    manifest_digest := binary(),
    config := binary(),
    config_digest := binary(),
    layers := [#{
        digest := binary(),
        size := non_neg_integer(),
        get_data := fun(() -> {ok, binary()} | {error, term()})
    }],
    platform => ocibuild:platform(),
    annotations => map()
}.

%% Load tarball entirely into memory
-spec load_tarball_to_memory(file:filename()) ->
    {ok, #{images := [loaded_image()], tag := binary() | undefined,
           is_multi_platform := boolean(), cleanup := fun(() -> ok)}} |
    {error, term()}.
load_tarball_to_memory(Path) ->
    case erl_tar:extract(Path, [memory, compressed]) of
        {ok, FileList} ->
            %% Normalize paths: remove leading "./" if present
            Files = maps:from_list([{normalize_tar_path(Name), Data} || {Name, Data} <- FileList]),
            GetBlob = fun(Digest) ->
                BlobPath = <<"blobs/sha256/", (ocibuild_digest:encoded(Digest))/binary>>,
                case maps:find(BlobPath, Files) of
                    {ok, Data} -> {ok, Data};
                    error -> {error, {missing_blob, Digest}}
                end
            end,
            parse_tarball_contents(Files, GetBlob, fun() -> ok end);
        {error, Reason} ->
            {error, {invalid_tarball, Reason}}
    end.

%% Normalize tar paths by removing leading "./" prefix
-spec normalize_tar_path(string()) -> binary().
normalize_tar_path("./" ++ Rest) ->
    list_to_binary(Rest);
normalize_tar_path(Path) ->
    list_to_binary(Path).

%% Load tarball to temp directory for memory efficiency
-spec load_tarball_to_disk(file:filename()) ->
    {ok, #{images := [loaded_image()], tag := binary() | undefined,
           is_multi_platform := boolean(), cleanup := fun(() -> ok)}} |
    {error, term()}.
load_tarball_to_disk(Path) ->
    TempDir = make_temp_dir(),
    case erl_tar:extract(Path, [{cwd, TempDir}, compressed]) of
        ok ->
            GetBlob = fun(Digest) ->
                BlobPath = filename:join([TempDir, "blobs", "sha256",
                                          binary_to_list(ocibuild_digest:encoded(Digest))]),
                file:read_file(BlobPath)
            end,
            %% Read index.json and oci-layout from disk
            case read_disk_files(TempDir) of
                {ok, Files} ->
                    Cleanup = fun() -> delete_temp_dir(TempDir) end,
                    parse_tarball_contents(Files, GetBlob, Cleanup);
                {error, _} = Err ->
                    delete_temp_dir(TempDir),
                    Err
            end;
        {error, Reason} ->
            delete_temp_dir(TempDir),
            {error, {invalid_tarball, Reason}}
    end.

%% Read metadata files from disk-extracted tarball
-spec read_disk_files(file:filename()) -> {ok, #{binary() => binary()}} | {error, term()}.
read_disk_files(TempDir) ->
    maybe
        {ok, IndexJson} ?= file:read_file(filename:join(TempDir, "index.json")),
        {ok, OciLayout} ?= file:read_file(filename:join(TempDir, "oci-layout")),
        {ok, #{~"index.json" => IndexJson, ~"oci-layout" => OciLayout}}
    end.

%% Parse tarball contents and build loaded images
-spec parse_tarball_contents(
    #{binary() => binary()},
    fun((binary()) -> {ok, binary()} | {error, term()}),
    fun(() -> ok)
) ->
    {ok, #{images := [loaded_image()], tag := binary() | undefined,
           is_multi_platform := boolean(), cleanup := fun(() -> ok)}} |
    {error, term()}.
parse_tarball_contents(Files, GetBlob, Cleanup) ->
    maybe
        %% Validate oci-layout
        ok ?= validate_oci_layout(Files),
        %% Parse index.json
        {ok, Index} ?= parse_index(Files),
        %% Extract tag from annotations
        Tag = extract_tag_from_index(Index),
        %% Get manifest entries
        Manifests = maps:get(~"manifests", Index, []),
        IsMultiPlatform = length(Manifests) > 1,
        %% Load each image
        {ok, Images} ?= load_images_from_manifests(Manifests, GetBlob),
        {ok, #{
            images => Images,
            tag => Tag,
            is_multi_platform => IsMultiPlatform,
            cleanup => Cleanup
        }}
    else
        {error, Reason} ->
            Cleanup(),
            {error, Reason}
    end.

%% Validate oci-layout file
-spec validate_oci_layout(#{binary() => binary()}) -> ok | {error, term()}.
validate_oci_layout(Files) ->
    case maps:find(~"oci-layout", Files) of
        {ok, OciLayoutJson} ->
            case ocibuild_json:decode(OciLayoutJson) of
                #{~"imageLayoutVersion" := ~"1.0.0"} -> ok;
                #{~"imageLayoutVersion" := Version} -> {error, {unsupported_layout_version, Version}};
                _ -> {error, {invalid_oci_layout, missing_version}}
            end;
        error ->
            {error, {invalid_oci_layout, missing_oci_layout}}
    end.

%% Parse index.json
-spec parse_index(#{binary() => binary()}) -> {ok, map()} | {error, term()}.
parse_index(Files) ->
    case maps:find(~"index.json", Files) of
        {ok, IndexJson} ->
            Index = ocibuild_json:decode(IndexJson),
            {ok, Index};
        error ->
            {error, {invalid_oci_layout, missing_index}}
    end.

%% Extract tag from index annotations (various locations)
-spec extract_tag_from_index(map()) -> binary() | undefined.
extract_tag_from_index(Index) ->
    %% Try top-level annotations first (multi-platform images)
    case maps:find(~"annotations", Index) of
        {ok, Annotations} ->
            case maps:find(~"org.opencontainers.image.ref.name", Annotations) of
                {ok, Tag} -> Tag;
                error -> extract_tag_from_manifests(Index)
            end;
        error ->
            extract_tag_from_manifests(Index)
    end.

%% Extract tag from manifest-level annotations
-spec extract_tag_from_manifests(map()) -> binary() | undefined.
extract_tag_from_manifests(#{~"manifests" := [#{~"annotations" := Ann} | _]}) ->
    maps:get(~"org.opencontainers.image.ref.name", Ann, undefined);
extract_tag_from_manifests(_) ->
    undefined.

%% Load images from manifest entries
-spec load_images_from_manifests(
    [map()],
    fun((binary()) -> {ok, binary()} | {error, term()})
) -> {ok, [loaded_image()]} | {error, term()}.
load_images_from_manifests(ManifestEntries, GetBlob) ->
    load_images_from_manifests(ManifestEntries, GetBlob, []).

load_images_from_manifests([], _GetBlob, Acc) ->
    {ok, lists:reverse(Acc)};
load_images_from_manifests([Entry | Rest], GetBlob, Acc) ->
    case load_single_image(Entry, GetBlob) of
        {ok, Image} ->
            load_images_from_manifests(Rest, GetBlob, [Image | Acc]);
        {error, _} = Err ->
            Err
    end.

%% Load a single image from manifest entry
-spec load_single_image(map(), fun((binary()) -> {ok, binary()} | {error, term()})) ->
    {ok, loaded_image()} | {error, term()}.
load_single_image(#{~"digest" := ManifestDigest} = Entry, GetBlob) ->
    maybe
        %% Load manifest
        {ok, ManifestJson} ?= GetBlob(ManifestDigest),
        Manifest = ocibuild_json:decode(ManifestJson),
        %% Load config
        #{~"digest" := ConfigDigest} = maps:get(~"config", Manifest),
        {ok, ConfigJson} ?= GetBlob(ConfigDigest),
        %% Build layer list with lazy loaders
        ManifestLayers = maps:get(~"layers", Manifest, []),
        Layers = [
            #{
                digest => LayerDigest,
                size => Size,
                get_data => fun() -> GetBlob(LayerDigest) end
            }
         || #{~"digest" := LayerDigest, ~"size" := Size} <- ManifestLayers
        ],
        %% Extract platform if present
        Platform = extract_platform(Entry),
        Annotations = maps:get(~"annotations", Manifest, #{}),
        {ok, #{
            manifest => ManifestJson,
            manifest_digest => ManifestDigest,
            config => ConfigJson,
            config_digest => ConfigDigest,
            layers => Layers,
            platform => Platform,
            annotations => Annotations
        }}
    end.

%% Extract platform from manifest entry
-spec extract_platform(map()) -> ocibuild:platform() | undefined.
extract_platform(#{~"platform" := #{~"os" := Os, ~"architecture" := Arch} = P}) ->
    Base = #{os => Os, architecture => Arch},
    case maps:find(~"variant", P) of
        {ok, Variant} -> Base#{variant => Variant};
        error -> Base
    end;
extract_platform(_) ->
    undefined.

%% Create a temporary directory
-spec make_temp_dir() -> file:filename().
make_temp_dir() ->
    TempBase = filename:join([filename:basedir(user_cache, "ocibuild"), "tmp"]),
    ok = filelib:ensure_dir(filename:join(TempBase, "dummy")),
    Unique = integer_to_list(erlang:unique_integer([positive])),
    TempDir = filename:join(TempBase, "load_" ++ Unique),
    ok = file:make_dir(TempDir),
    TempDir.

%% Delete temporary directory recursively
-spec delete_temp_dir(file:filename()) -> ok.
delete_temp_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(
                fun(File) ->
                    Path = filename:join(Dir, File),
                    case filelib:is_dir(Path) of
                        true -> delete_temp_dir(Path);
                        false -> file:delete(Path)
                    end
                end,
                Files
            ),
            file:del_dir(Dir),
            ok;
        {error, _} ->
            ok
    end.

-ifdef(TEST).
%% Extract tarball contents (for testing)
-spec extract_tarball(file:filename()) -> {ok, [{string(), binary()}]} | {error, term()}.
extract_tarball(Path) ->
    erl_tar:extract(Path, [memory, compressed]).
-endif.

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

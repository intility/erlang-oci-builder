%%%-------------------------------------------------------------------
%%% @doc
%%% OCI image layout handling.
%%%
%%% Produces OCI Image Layout format for use with `docker load',
%%% `podman load', or direct filesystem storage.
%%%
%%% The layout structure is:
%%% ```
%%% myimage/
%%% ├── oci-layout           # {"imageLayoutVersion": "1.0.0"}
%%% ├── index.json           # Entry point
%%% └── blobs/
%%%     └── sha256/
%%%         ├── <manifest>   # Manifest JSON
%%%         ├── <config>     # Config JSON
%%%         └── <layers...>  # Layer tarballs
%%% '''
%%%
%%% See: https://github.com/opencontainers/image-spec/blob/main/image-layout.md
%%% @end
%%%-------------------------------------------------------------------
-module(ocibuild_layout).

-export([
    export_directory/2,
    save_tarball/2
]).

%% @doc Export image as an OCI layout directory.
%%
%% Creates the standard OCI directory structure at the given path.
%% ```
%% ok = ocibuild_layout:export_directory(Image, "./myimage").
%% '''
-spec export_directory(ocibuild:image(), file:filename()) -> ok | {error, term()}.
export_directory(Image, Path) ->
    try
        %% Create directory structure
        BlobsDir = filename:join([Path, "blobs", "sha256"]),
        ok = filelib:ensure_dir(filename:join(BlobsDir, "dummy")),
        
        %% Build all the components
        {ConfigJson, ConfigDigest} = build_config_blob(Image),
        LayerDescriptors = build_layer_descriptors(Image),
        {ManifestJson, ManifestDigest} = ocibuild_manifest:build(
            #{
                <<"mediaType">> => <<"application/vnd.oci.image.config.v1+json">>,
                <<"digest">> => ConfigDigest,
                <<"size">> => byte_size(ConfigJson)
            },
            LayerDescriptors
        ),
        
        %% Write oci-layout
        OciLayout = ocibuild_json:encode(#{<<"imageLayoutVersion">> => <<"1.0.0">>}),
        ok = file:write_file(filename:join(Path, "oci-layout"), OciLayout),
        
        %% Write index.json
        Index = build_index(ManifestDigest, byte_size(ManifestJson)),
        IndexJson = ocibuild_json:encode(Index),
        ok = file:write_file(filename:join(Path, "index.json"), IndexJson),
        
        %% Write config blob
        ConfigPath = filename:join(BlobsDir, ocibuild_digest:encoded(ConfigDigest)),
        ok = file:write_file(ConfigPath, ConfigJson),
        
        %% Write manifest blob
        ManifestPath = filename:join(BlobsDir, ocibuild_digest:encoded(ManifestDigest)),
        ok = file:write_file(ManifestPath, ManifestJson),
        
        %% Write layer blobs
        lists:foreach(
            fun(#{digest := Digest, data := Data}) ->
                LayerPath = filename:join(BlobsDir, ocibuild_digest:encoded(Digest)),
                ok = file:write_file(LayerPath, Data)
            end,
            maps:get(layers, Image, [])
        ),
        
        %% Write base image layers if present (reference only, not data)
        %% For full functionality, we'd need to fetch and include base layers
        
        ok
    catch
        error:Reason ->
            {error, Reason}
    end.

%% @doc Save image as an OCI layout tarball.
%%
%% Creates a tar.gz file that can be loaded with `docker load' or `podman load'.
%% ```
%% ok = ocibuild_layout:save_tarball(Image, "./myimage.tar.gz").
%% '''
-spec save_tarball(ocibuild:image(), file:filename()) -> ok | {error, term()}.
save_tarball(Image, Path) ->
    try
        %% Build all the blobs and metadata
        {ConfigJson, ConfigDigest} = build_config_blob(Image),
        LayerDescriptors = build_layer_descriptors(Image),
        {ManifestJson, ManifestDigest} = ocibuild_manifest:build(
            #{
                <<"mediaType">> => <<"application/vnd.oci.image.config.v1+json">>,
                <<"digest">> => ConfigDigest,
                <<"size">> => byte_size(ConfigJson)
            },
            LayerDescriptors
        ),
        
        %% Build oci-layout
        OciLayout = ocibuild_json:encode(#{<<"imageLayoutVersion">> => <<"1.0.0">>}),
        
        %% Build index.json
        Index = build_index(ManifestDigest, byte_size(ManifestJson)),
        IndexJson = ocibuild_json:encode(Index),
        
        %% Collect all files for the tarball
        Files = [
            {<<"oci-layout">>, OciLayout, 8#644},
            {<<"index.json">>, IndexJson, 8#644},
            {blob_path(ConfigDigest), ConfigJson, 8#644},
            {blob_path(ManifestDigest), ManifestJson, 8#644}
        ] ++ [
            {blob_path(Digest), Data, 8#644}
            || #{digest := Digest, data := Data} <- maps:get(layers, Image, [])
        ],
        
        %% Create the tarball
        TarData = ocibuild_tar:create(Files),
        Compressed = zlib:gzip(TarData),
        
        ok = file:write_file(Path, Compressed)
    catch
        error:Reason ->
            {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Build the config JSON blob
-spec build_config_blob(ocibuild:image()) -> {binary(), binary()}.
build_config_blob(#{config := Config}) ->
    Json = ocibuild_json:encode(Config),
    Digest = ocibuild_digest:sha256(Json),
    {Json, Digest}.

%% Build layer descriptors for the manifest
-spec build_layer_descriptors(ocibuild:image()) -> [map()].
build_layer_descriptors(#{base_manifest := BaseManifest, layers := NewLayers}) ->
    %% Include base image layers + new layers
    BaseLayers = maps:get(<<"layers">>, BaseManifest, []),
    NewDescriptors = [
        #{
            <<"mediaType">> => MediaType,
            <<"digest">> => Digest,
            <<"size">> => Size
        }
        || #{media_type := MediaType, digest := Digest, size := Size} <- NewLayers
    ],
    BaseLayers ++ NewDescriptors;
build_layer_descriptors(#{layers := NewLayers}) ->
    %% No base image, just new layers
    [
        #{
            <<"mediaType">> => MediaType,
            <<"digest">> => Digest,
            <<"size">> => Size
        }
        || #{media_type := MediaType, digest := Digest, size := Size} <- NewLayers
    ].

%% Build the index.json structure
-spec build_index(binary(), non_neg_integer()) -> map().
build_index(ManifestDigest, ManifestSize) ->
    #{
        <<"schemaVersion">> => 2,
        <<"manifests">> => [
            #{
                <<"mediaType">> => <<"application/vnd.oci.image.manifest.v1+json">>,
                <<"digest">> => ManifestDigest,
                <<"size">> => ManifestSize
            }
        ]
    }.

%% Convert digest to blob path
-spec blob_path(binary()) -> binary().
blob_path(Digest) ->
    Encoded = ocibuild_digest:encoded(Digest),
    <<"blobs/sha256/", Encoded/binary>>.

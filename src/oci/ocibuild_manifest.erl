%%%-------------------------------------------------------------------
-module(ocibuild_manifest).
-moduledoc """
OCI image manifest builder.

The manifest ties together the config and layers that make up an image.
Compliant with the OCI Image Specification.

See: https://github.com/opencontainers/image-spec/blob/main/manifest.md
""".

-export([build/2, build/3]).
-export([media_type/0, config_media_type/0, layer_media_type/0, layer_media_type/1]).

-type descriptor() ::
    #{
        mediaType := binary(),
        digest := binary(),
        size := non_neg_integer()
    }.
-type manifest() ::
    #{
        schemaVersion := integer(),
        mediaType := binary(),
        config := descriptor(),
        layers := [descriptor()],
        annotations => map()
    }.

-export_type([manifest/0, descriptor/0]).

-doc "Build an OCI image manifest from config and layer descriptors.".
-spec build(descriptor(), [descriptor()]) -> {binary(), binary()}.
build(ConfigDescriptor, LayerDescriptors) ->
    build(ConfigDescriptor, LayerDescriptors, #{}).

-doc "Build an OCI image manifest from config and layer descriptors with annotations.".
-spec build(descriptor(), [descriptor()], map()) -> {binary(), binary()}.
build(ConfigDescriptor, LayerDescriptors, Annotations) ->
    BaseManifest =
        #{
            ~"schemaVersion" => 2,
            ~"mediaType" => media_type(),
            ~"config" => ConfigDescriptor,
            ~"layers" => LayerDescriptors
        },
    Manifest =
        case map_size(Annotations) of
            0 -> BaseManifest;
            _ -> BaseManifest#{~"annotations" => Annotations}
        end,
    Json = ocibuild_json:encode(Manifest),
    Digest = ocibuild_digest:sha256(Json),
    {Json, Digest}.

-doc "OCI image manifest media type.".
-spec media_type() -> binary().
media_type() ->
    ~"application/vnd.oci.image.manifest.v1+json".

-doc "OCI image config media type.".
-spec config_media_type() -> binary().
config_media_type() ->
    ~"application/vnd.oci.image.config.v1+json".

-doc "OCI layer media type (gzip compressed).".
-spec layer_media_type() -> binary().
layer_media_type() ->
    layer_media_type(gzip).

-doc "OCI layer media type for specified compression.".
-spec layer_media_type(gzip | zstd | none) -> binary().
layer_media_type(gzip) ->
    ~"application/vnd.oci.image.layer.v1.tar+gzip";
layer_media_type(zstd) ->
    ~"application/vnd.oci.image.layer.v1.tar+zstd";
layer_media_type(none) ->
    ~"application/vnd.oci.image.layer.v1.tar".

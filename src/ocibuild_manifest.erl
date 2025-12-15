%%%-------------------------------------------------------------------
%%% @doc
%%% OCI image manifest builder.
%%%
%%% The manifest ties together the config and layers that make up an image.
%%% See: https://github.com/opencontainers/image-spec/blob/main/manifest.md
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ocibuild_manifest).

-export([
    build/2,
    media_type/0,
    config_media_type/0
]).

-type descriptor() :: #{
    mediaType := binary(),
    digest := binary(),
    size := non_neg_integer()
}.

-type manifest() :: #{
    schemaVersion := integer(),
    mediaType := binary(),
    config := descriptor(),
    layers := [descriptor()]
}.

-export_type([manifest/0, descriptor/0]).

%% @doc Build an OCI image manifest.
%%
%% Takes the config descriptor and layer descriptors and produces
%% a complete OCI manifest.
-spec build(descriptor(), [descriptor()]) -> {binary(), binary()}.
build(ConfigDescriptor, LayerDescriptors) ->
    Manifest = #{
        <<"schemaVersion">> => 2,
        <<"mediaType">> => media_type(),
        <<"config">> => ConfigDescriptor,
        <<"layers">> => LayerDescriptors
    },
    Json = ocibuild_json:encode(Manifest),
    Digest = ocibuild_digest:sha256(Json),
    {Json, Digest}.

%% @doc Return the OCI image manifest media type.
-spec media_type() -> binary().
media_type() ->
    <<"application/vnd.oci.image.manifest.v1+json">>.

%% @doc Return the OCI image config media type.
-spec config_media_type() -> binary().
config_media_type() ->
    <<"application/vnd.oci.image.config.v1+json">>.

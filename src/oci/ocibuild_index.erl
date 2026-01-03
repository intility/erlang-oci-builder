-module(ocibuild_index).
-moduledoc """
`ocibuild_index` - OCI Image Index creation and manipulation.

This module handles OCI Image Index (multi-platform manifest list) operations.
An image index allows a single tag to reference multiple platform-specific images.

See: https://github.com/opencontainers/image-spec/blob/main/image-index.md
""".

-export([create/1, to_json/1, parse/1, select_manifest/2]).
-export([media_type/0]).
-export([matches_platform/2]).

-export_type([index/0, manifest_descriptor/0]).

-type platform() :: ocibuild:platform().

-type manifest_descriptor() ::
    #{
        media_type := binary(),
        digest := binary(),
        size := non_neg_integer(),
        platform := platform(),
        annotations => map()
    }.

-opaque index() ::
    #{
        schema_version := 2,
        media_type := binary(),
        manifests := [manifest_descriptor()],
        annotations => map()
    }.

-define(INDEX_MEDIA_TYPE, ~"application/vnd.oci.image.index.v1+json").
-define(MANIFEST_MEDIA_TYPE, ~"application/vnd.oci.image.manifest.v1+json").

%%%===================================================================
%%% API
%%%===================================================================

-doc "Return the OCI media type for image indexes.".
-spec media_type() -> binary().
media_type() ->
    ?INDEX_MEDIA_TYPE.

-doc """
Create an OCI image index from a list of platform-specific manifest descriptors.

Each descriptor is a tuple of `{Platform, ManifestDigest, ManifestSize}`.

```
Index = ocibuild_index:create([
    {#{os => ~"linux", architecture => ~"amd64"}, ~"sha256:abc...", 1234},
    {#{os => ~"linux", architecture => ~"arm64"}, ~"sha256:def...", 1235}
]).
```
""".
-spec create([{platform(), Digest :: binary(), Size :: non_neg_integer()}]) -> index().
create(PlatformManifests) ->
    Manifests = [
        #{
            media_type => ?MANIFEST_MEDIA_TYPE,
            digest => Digest,
            size => Size,
            platform => Platform
        }
     || {Platform, Digest, Size} <- PlatformManifests
    ],
    #{
        schema_version => 2,
        media_type => ?INDEX_MEDIA_TYPE,
        manifests => Manifests
    }.

-doc """
Serialize an image index to JSON binary.

The output conforms to the OCI Image Index specification.
""".
-spec to_json(index()) -> binary().
to_json(
    #{schema_version := SchemaVersion, media_type := MediaType, manifests := Manifests} = Index
) ->
    JsonManifests = [manifest_descriptor_to_json(M) || M <- Manifests],
    JsonIndex0 = #{
        ~"schemaVersion" => SchemaVersion,
        ~"mediaType" => MediaType,
        ~"manifests" => JsonManifests
    },
    JsonIndex =
        case maps:find(annotations, Index) of
            {ok, Annotations} when map_size(Annotations) > 0 ->
                JsonIndex0#{~"annotations" => Annotations};
            _ ->
                JsonIndex0
        end,
    ocibuild_json:encode(JsonIndex).

-doc """
Parse a JSON binary into an image index.

```
{ok, Index} = ocibuild_index:parse(JsonBinary).
```
""".
-spec parse(binary()) -> {ok, index()} | {error, term()}.
parse(JsonBinary) ->
    try
        Json = ocibuild_json:decode(JsonBinary),
        SchemaVersion = maps:get(~"schemaVersion", Json),
        MediaType = maps:get(~"mediaType", Json, ?INDEX_MEDIA_TYPE),
        Manifests = [parse_manifest_descriptor(M) || M <- maps:get(~"manifests", Json, [])],
        Index0 = #{
            schema_version => SchemaVersion,
            media_type => MediaType,
            manifests => Manifests
        },
        Index =
            case maps:find(~"annotations", Json) of
                {ok, Annotations} ->
                    Index0#{annotations => Annotations};
                error ->
                    Index0
            end,
        {ok, Index}
    catch
        _:Reason ->
            {error, {parse_failed, Reason}}
    end.

-doc """
Select a manifest descriptor matching the given platform.

Returns the first manifest that matches both OS and architecture.
If the platform specifies a variant, that is also matched.

```
{ok, Descriptor} = ocibuild_index:select_manifest(Index,
    #{os => ~"linux", architecture => ~"arm64"}).
```
""".
-spec select_manifest(index(), platform()) -> {ok, manifest_descriptor()} | {error, not_found}.
select_manifest(#{manifests := Manifests}, TargetPlatform) ->
    TargetOs = maps:get(os, TargetPlatform),
    TargetArch = maps:get(architecture, TargetPlatform),
    TargetVariant = maps:get(variant, TargetPlatform, undefined),
    find_matching_manifest(Manifests, TargetOs, TargetArch, TargetVariant).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec manifest_descriptor_to_json(manifest_descriptor()) -> map().
manifest_descriptor_to_json(
    #{media_type := MediaType, digest := Digest, size := Size, platform := Platform} = Desc
) ->
    JsonPlatform = platform_to_json(Platform),
    Json0 = #{
        ~"mediaType" => MediaType,
        ~"digest" => Digest,
        ~"size" => Size,
        ~"platform" => JsonPlatform
    },
    case maps:find(annotations, Desc) of
        {ok, Annotations} when map_size(Annotations) > 0 ->
            Json0#{~"annotations" => Annotations};
        _ ->
            Json0
    end.

-spec platform_to_json(platform()) -> map().
platform_to_json(Platform) ->
    Base = #{
        ~"os" => maps:get(os, Platform),
        ~"architecture" => maps:get(architecture, Platform)
    },
    WithVariant =
        case maps:find(variant, Platform) of
            {ok, Variant} -> Base#{~"variant" => Variant};
            error -> Base
        end,
    case maps:find(os_version, Platform) of
        {ok, OsVersion} -> WithVariant#{~"os.version" => OsVersion};
        error -> WithVariant
    end.

-spec parse_manifest_descriptor(map()) -> manifest_descriptor().
parse_manifest_descriptor(Json) ->
    Platform = parse_platform(maps:get(~"platform", Json, #{})),
    Desc0 = #{
        media_type => maps:get(~"mediaType", Json),
        digest => maps:get(~"digest", Json),
        size => maps:get(~"size", Json),
        platform => Platform
    },
    case maps:find(~"annotations", Json) of
        {ok, Annotations} ->
            Desc0#{annotations => Annotations};
        error ->
            Desc0
    end.

-spec parse_platform(map()) -> platform().
parse_platform(Json) ->
    Base = #{
        os => maps:get(~"os", Json, ~"linux"),
        architecture => maps:get(~"architecture", Json, ~"amd64")
    },
    WithVariant =
        case maps:find(~"variant", Json) of
            {ok, Variant} -> Base#{variant => Variant};
            error -> Base
        end,
    case maps:find(~"os.version", Json) of
        {ok, OsVersion} -> WithVariant#{os_version => OsVersion};
        error -> WithVariant
    end.

-spec find_matching_manifest([manifest_descriptor()], binary(), binary(), binary() | undefined) ->
    {ok, manifest_descriptor()} | {error, not_found}.
find_matching_manifest([], _Os, _Arch, _Variant) ->
    {error, not_found};
find_matching_manifest(
    [#{platform := Platform} = Desc | Rest], TargetOs, TargetArch, TargetVariant
) ->
    Os = maps:get(os, Platform),
    Arch = maps:get(architecture, Platform),
    Variant = maps:get(variant, Platform, undefined),
    case matches_platform_internal(Os, Arch, Variant, TargetOs, TargetArch, TargetVariant) of
        true ->
            {ok, Desc};
        false ->
            find_matching_manifest(Rest, TargetOs, TargetArch, TargetVariant)
    end.

-doc """
Check if a platform matches a target platform.

Matches are based on OS and architecture. If the target specifies a variant,
that must also match.

Example:
```
true = ocibuild_index:matches_platform(
    #{os => ~"linux", architecture => ~"amd64"},
    #{os => ~"linux", architecture => ~"amd64"}
).
```
""".
-spec matches_platform(platform(), platform()) -> boolean().
matches_platform(Platform, Target) ->
    Os = maps:get(os, Platform),
    Arch = maps:get(architecture, Platform),
    Variant = maps:get(variant, Platform, undefined),
    TargetOs = maps:get(os, Target),
    TargetArch = maps:get(architecture, Target),
    TargetVariant = maps:get(variant, Target, undefined),
    matches_platform_internal(Os, Arch, Variant, TargetOs, TargetArch, TargetVariant).

%% @private Internal platform matching
-spec matches_platform_internal(
    binary(), binary(), binary() | undefined, binary(), binary(), binary() | undefined
) -> boolean().
matches_platform_internal(Os, Arch, Variant, TargetOs, TargetArch, TargetVariant) ->
    Os =:= TargetOs andalso
        Arch =:= TargetArch andalso
        (TargetVariant =:= undefined orelse Variant =:= TargetVariant).

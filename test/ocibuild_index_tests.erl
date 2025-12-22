-module(ocibuild_index_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Index creation tests
%%%===================================================================

create_single_platform_test() ->
    Platform = #{os => ~"linux", architecture => ~"amd64"},
    Digest = ~"sha256:abc123",
    Size = 1234,

    Index = ocibuild_index:create([{Platform, Digest, Size}]),

    ?assertEqual(2, maps:get(schema_version, Index)),
    ?assertEqual(~"application/vnd.oci.image.index.v1+json", maps:get(media_type, Index)),

    [Manifest] = maps:get(manifests, Index),
    ?assertEqual(~"application/vnd.oci.image.manifest.v1+json", maps:get(media_type, Manifest)),
    ?assertEqual(Digest, maps:get(digest, Manifest)),
    ?assertEqual(Size, maps:get(size, Manifest)),
    ?assertEqual(Platform, maps:get(platform, Manifest)).

create_multiple_platforms_test() ->
    Platform1 = #{os => ~"linux", architecture => ~"amd64"},
    Platform2 = #{os => ~"linux", architecture => ~"arm64"},
    Platform3 = #{os => ~"linux", architecture => ~"arm64", variant => ~"v8"},

    Index = ocibuild_index:create([
        {Platform1, ~"sha256:amd64digest", 1000},
        {Platform2, ~"sha256:arm64digest", 1001},
        {Platform3, ~"sha256:arm64v8digest", 1002}
    ]),

    Manifests = maps:get(manifests, Index),
    ?assertEqual(3, length(Manifests)).

create_empty_test() ->
    Index = ocibuild_index:create([]),
    ?assertEqual([], maps:get(manifests, Index)).

%%%===================================================================
%%% JSON serialization tests
%%%===================================================================

to_json_format_test() ->
    Platform = #{os => ~"linux", architecture => ~"amd64"},
    Index = ocibuild_index:create([{Platform, ~"sha256:abc", 100}]),

    Json = ocibuild_index:to_json(Index),
    ?assert(is_binary(Json)),

    %% Parse it back to verify structure
    Decoded = ocibuild_json:decode(Json),
    ?assertEqual(2, maps:get(~"schemaVersion", Decoded)),
    ?assertEqual(~"application/vnd.oci.image.index.v1+json", maps:get(~"mediaType", Decoded)),

    [Manifest] = maps:get(~"manifests", Decoded),
    ?assertEqual(~"sha256:abc", maps:get(~"digest", Manifest)),
    ?assertEqual(100, maps:get(~"size", Manifest)),

    DecodedPlatform = maps:get(~"platform", Manifest),
    ?assertEqual(~"linux", maps:get(~"os", DecodedPlatform)),
    ?assertEqual(~"amd64", maps:get(~"architecture", DecodedPlatform)).

to_json_with_variant_test() ->
    Platform = #{os => ~"linux", architecture => ~"arm64", variant => ~"v8"},
    Index = ocibuild_index:create([{Platform, ~"sha256:abc", 100}]),

    Json = ocibuild_index:to_json(Index),
    Decoded = ocibuild_json:decode(Json),

    [Manifest] = maps:get(~"manifests", Decoded),
    DecodedPlatform = maps:get(~"platform", Manifest),
    ?assertEqual(~"v8", maps:get(~"variant", DecodedPlatform)).

%%%===================================================================
%%% Parse tests
%%%===================================================================

parse_roundtrip_test() ->
    Platform1 = #{os => ~"linux", architecture => ~"amd64"},
    Platform2 = #{os => ~"linux", architecture => ~"arm64", variant => ~"v8"},

    Original = ocibuild_index:create([
        {Platform1, ~"sha256:amd64", 1000},
        {Platform2, ~"sha256:arm64v8", 1001}
    ]),

    Json = ocibuild_index:to_json(Original),
    {ok, Parsed} = ocibuild_index:parse(Json),

    ?assertEqual(maps:get(schema_version, Original), maps:get(schema_version, Parsed)),
    ?assertEqual(maps:get(media_type, Original), maps:get(media_type, Parsed)),
    ?assertEqual(length(maps:get(manifests, Original)), length(maps:get(manifests, Parsed))).

parse_invalid_json_test() ->
    ?assertMatch({error, _}, ocibuild_index:parse(~"not json")),
    ?assertMatch({error, _}, ocibuild_index:parse(~"{broken")).

parse_real_world_index_test() ->
    %% A realistic index JSON from a registry
    Json =
        <<
            "{\n"
            "        \"schemaVersion\": 2,\n"
            "        \"mediaType\": \"application/vnd.oci.image.index.v1+json\",\n"
            "        \"manifests\": [\n"
            "            {\n"
            "                \"mediaType\": \"application/vnd.oci.image.manifest.v1+json\",\n"
            "                \"digest\": \"sha256:1234567890abcdef\",\n"
            "                \"size\": 1234,\n"
            "                \"platform\": {\n"
            "                    \"os\": \"linux\",\n"
            "                    \"architecture\": \"amd64\"\n"
            "                }\n"
            "            },\n"
            "            {\n"
            "                \"mediaType\": \"application/vnd.oci.image.manifest.v1+json\",\n"
            "                \"digest\": \"sha256:abcdef1234567890\",\n"
            "                \"size\": 1235,\n"
            "                \"platform\": {\n"
            "                    \"os\": \"linux\",\n"
            "                    \"architecture\": \"arm64\",\n"
            "                    \"variant\": \"v8\"\n"
            "                }\n"
            "            }\n"
            "        ]\n"
            "    }"
        >>,

    {ok, Index} = ocibuild_index:parse(Json),
    ?assertEqual(2, maps:get(schema_version, Index)),

    Manifests = maps:get(manifests, Index),
    ?assertEqual(2, length(Manifests)),

    [Amd64, Arm64] = Manifests,
    ?assertEqual(~"amd64", maps:get(architecture, maps:get(platform, Amd64))),
    ?assertEqual(~"arm64", maps:get(architecture, maps:get(platform, Arm64))),
    ?assertEqual(~"v8", maps:get(variant, maps:get(platform, Arm64))).

%%%===================================================================
%%% Select manifest tests
%%%===================================================================

select_manifest_found_test() ->
    Platform1 = #{os => ~"linux", architecture => ~"amd64"},
    Platform2 = #{os => ~"linux", architecture => ~"arm64"},

    Index = ocibuild_index:create([
        {Platform1, ~"sha256:amd64digest", 1000},
        {Platform2, ~"sha256:arm64digest", 1001}
    ]),

    %% Find amd64
    {ok, Amd64} = ocibuild_index:select_manifest(Index, #{os => ~"linux", architecture => ~"amd64"}),
    ?assertEqual(~"sha256:amd64digest", maps:get(digest, Amd64)),

    %% Find arm64
    {ok, Arm64} = ocibuild_index:select_manifest(Index, #{os => ~"linux", architecture => ~"arm64"}),
    ?assertEqual(~"sha256:arm64digest", maps:get(digest, Arm64)).

select_manifest_not_found_test() ->
    Platform = #{os => ~"linux", architecture => ~"amd64"},
    Index = ocibuild_index:create([{Platform, ~"sha256:amd64", 1000}]),

    %% Try to find arm64 - should not be found
    ?assertEqual(
        {error, not_found},
        ocibuild_index:select_manifest(Index, #{os => ~"linux", architecture => ~"arm64"})
    ),

    %% Try to find different OS
    ?assertEqual(
        {error, not_found},
        ocibuild_index:select_manifest(Index, #{os => ~"windows", architecture => ~"amd64"})
    ).

select_manifest_with_variant_test() ->
    Platform1 = #{os => ~"linux", architecture => ~"arm64"},
    Platform2 = #{os => ~"linux", architecture => ~"arm64", variant => ~"v8"},

    Index = ocibuild_index:create([
        {Platform1, ~"sha256:arm64novariant", 1000},
        {Platform2, ~"sha256:arm64v8", 1001}
    ]),

    %% Without variant - should match first arm64
    {ok, NoVariant} = ocibuild_index:select_manifest(Index, #{
        os => ~"linux", architecture => ~"arm64"
    }),
    ?assertEqual(~"sha256:arm64novariant", maps:get(digest, NoVariant)),

    %% With variant - should match specific v8
    {ok, WithVariant} = ocibuild_index:select_manifest(
        Index,
        #{os => ~"linux", architecture => ~"arm64", variant => ~"v8"}
    ),
    ?assertEqual(~"sha256:arm64v8", maps:get(digest, WithVariant)),

    %% Wrong variant - should not find
    ?assertEqual(
        {error, not_found},
        ocibuild_index:select_manifest(
            Index,
            #{os => ~"linux", architecture => ~"arm64", variant => ~"v7"}
        )
    ).

select_manifest_empty_index_test() ->
    Index = ocibuild_index:create([]),
    ?assertEqual(
        {error, not_found},
        ocibuild_index:select_manifest(Index, #{os => ~"linux", architecture => ~"amd64"})
    ).

%%%===================================================================
%%% Media type test
%%%===================================================================

media_type_test() ->
    ?assertEqual(~"application/vnd.oci.image.index.v1+json", ocibuild_index:media_type()).

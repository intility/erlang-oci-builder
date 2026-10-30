%%%-------------------------------------------------------------------
-module(ocibuild_sbom_tests).
-moduledoc """
Tests for ocibuild_sbom module - SPDX 2.3 SBOM generation.
""".

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Basic SBOM Generation Tests
%%%===================================================================

sbom_basic_test() ->
    %% Test basic SBOM generation
    Deps = [#{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"}],
    Opts = #{app_name => ~"myapp", app_version => ~"1.0.0"},
    {ok, SbomJson} = ocibuild_sbom:generate(Deps, Opts),
    Sbom = ocibuild_json:decode(SbomJson),
    ?assertEqual(~"SPDX-2.3", maps:get(~"spdxVersion", Sbom)),
    ?assertEqual(~"SPDXRef-DOCUMENT", maps:get(~"SPDXID", Sbom)),
    ?assertEqual(~"CC0-1.0", maps:get(~"dataLicense", Sbom)),
    ?assertEqual(~"myapp-1.0.0", maps:get(~"name", Sbom)).

sbom_media_type_test() ->
    %% Test SBOM media type
    ?assertEqual(~"application/spdx+json", ocibuild_sbom:media_type()).

sbom_document_namespace_test() ->
    %% Test that namespace is properly formatted with deterministic hash
    Deps = [],
    Opts = #{app_name => ~"myapp", app_version => ~"1.0.0"},
    {ok, SbomJson} = ocibuild_sbom:generate(Deps, Opts),
    Sbom = ocibuild_json:decode(SbomJson),
    Namespace = maps:get(~"documentNamespace", Sbom),
    %% Namespace should be: https://spdx.org/spdxdocs/<doc-name>-<16-char-hash>
    ?assertMatch(<<"https://spdx.org/spdxdocs/myapp-1.0.0-", _:16/binary>>, Namespace).

sbom_with_release_name_test() ->
    %% Test SBOM with different app and release names
    Deps = [],
    Opts = #{app_name => ~"indicator_sync", release_name => ~"server", app_version => ~"1.0.0"},
    {ok, SbomJson} = ocibuild_sbom:generate(Deps, Opts),
    Sbom = ocibuild_json:decode(SbomJson),
    %% Document name should include both app and release names
    ?assertEqual(~"indicator_sync-server-1.0.0", maps:get(~"name", Sbom)).

sbom_creation_info_test() ->
    %% Test that creation info is properly set
    os:putenv("SOURCE_DATE_EPOCH", "1700000000"),
    try
        Deps = [],
        Opts = #{app_name => ~"myapp", app_version => ~"1.0.0"},
        {ok, SbomJson} = ocibuild_sbom:generate(Deps, Opts),
        Sbom = ocibuild_json:decode(SbomJson),
        CreationInfo = maps:get(~"creationInfo", Sbom),
        ?assertEqual(~"2023-11-14T22:13:20Z", maps:get(~"created", CreationInfo)),
        Creators = maps:get(~"creators", CreationInfo),
        ?assertEqual(1, length(Creators)),
        [Creator] = Creators,
        ?assertMatch(<<"Tool: ocibuild-", _/binary>>, Creator)
    after
        os:unsetenv("SOURCE_DATE_EPOCH")
    end.

%%%===================================================================
%%% PURL Generation Tests
%%%===================================================================

sbom_purl_hex_test() ->
    %% Test PURL generation for hex packages
    Dep = #{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"},
    ?assertEqual(~"pkg:hex/cowboy@2.10.0", ocibuild_sbom:to_purl(Dep)).

sbom_purl_hexpm_test() ->
    %% Test PURL generation for hexpm packages (Mix uses "hexpm")
    Dep = #{name => ~"phoenix", version => ~"1.7.0", source => ~"hexpm"},
    ?assertEqual(~"pkg:hex/phoenix@1.7.0", ocibuild_sbom:to_purl(Dep)).

sbom_purl_github_test() ->
    %% Test PURL generation for GitHub dependencies
    Dep = #{name => ~"my_lib", version => ~"1.0.0", source => ~"https://github.com/owner/repo.git"},
    ?assertEqual(~"pkg:github/owner/repo@1.0.0", ocibuild_sbom:to_purl(Dep)).

sbom_purl_gitlab_test() ->
    %% Test PURL generation for GitLab dependencies
    Dep = #{name => ~"my_lib", version => ~"2.0.0", source => ~"https://gitlab.com/owner/repo.git"},
    ?assertEqual(~"pkg:gitlab/owner/repo@2.0.0", ocibuild_sbom:to_purl(Dep)).

sbom_purl_bitbucket_test() ->
    %% Test PURL generation for Bitbucket dependencies
    Dep = #{
        name => ~"my_lib", version => ~"3.0.0", source => ~"https://bitbucket.org/owner/repo.git"
    },
    ?assertEqual(~"pkg:bitbucket/owner/repo@3.0.0", ocibuild_sbom:to_purl(Dep)).

sbom_purl_generic_test() ->
    %% Test PURL generation for unknown sources
    Dep = #{name => ~"unknown", version => ~"1.0.0", source => ~"local"},
    ?assertEqual(~"pkg:generic/unknown@1.0.0", ocibuild_sbom:to_purl(Dep)).

%%%===================================================================
%%% Package Tests
%%%===================================================================

sbom_packages_test() ->
    %% Test that SBOM includes all dependencies as packages
    Deps = [
        #{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"},
        #{name => ~"plug", version => ~"1.14.0", source => ~"hex"}
    ],
    Opts = #{app_name => ~"myapp", app_version => ~"1.0.0"},
    {ok, SbomJson} = ocibuild_sbom:generate(Deps, Opts),
    Sbom = ocibuild_json:decode(SbomJson),
    Packages = maps:get(~"packages", Sbom),
    %% Should have 3 packages: myapp + cowboy + plug
    ?assertEqual(3, length(Packages)),
    %% Verify main package
    MainPkg = lists:filter(
        fun(P) -> maps:get(~"SPDXID", P) =:= ~"SPDXRef-Package-myapp" end,
        Packages
    ),
    ?assertEqual(1, length(MainPkg)).

sbom_with_erts_test() ->
    %% Test SBOM with ERTS version
    Deps = [],
    Opts = #{
        app_name => ~"myapp",
        app_version => ~"1.0.0",
        erts_version => ~"14.2",
        otp_version => ~"27.0"
    },
    {ok, SbomJson} = ocibuild_sbom:generate(Deps, Opts),
    Sbom = ocibuild_json:decode(SbomJson),
    Packages = maps:get(~"packages", Sbom),
    %% Should have 2 packages: myapp + erts
    ?assertEqual(2, length(Packages)),
    ErtsPkg = lists:filter(
        fun(P) -> maps:get(~"SPDXID", P) =:= ~"SPDXRef-Package-erts" end,
        Packages
    ),
    ?assertEqual(1, length(ErtsPkg)).

sbom_with_base_image_test() ->
    %% Test SBOM with base image
    Deps = [],
    Opts = #{
        app_name => ~"myapp",
        app_version => ~"1.0.0",
        base_image => {~"ghcr.io", ~"myorg/base", ~"v1"},
        base_digest => ~"sha256:abc123"
    },
    {ok, SbomJson} = ocibuild_sbom:generate(Deps, Opts),
    Sbom = ocibuild_json:decode(SbomJson),
    Packages = maps:get(~"packages", Sbom),
    %% Should have 2 packages: myapp + base-image
    ?assertEqual(2, length(Packages)),
    BasePkg = lists:filter(
        fun(P) -> maps:get(~"SPDXID", P) =:= ~"SPDXRef-Package-base-image" end,
        Packages
    ),
    ?assertEqual(1, length(BasePkg)).

%%%===================================================================
%%% Download Location Tests
%%%===================================================================

download_location_test_() ->
    Loc = fun(Source, Version) ->
        {ok, Json} = ocibuild_sbom:generate(
            [#{name => ~"dep", version => Version, source => Source}],
            #{app_name => ~"myapp", app_version => ~"1.0.0"}
        ),
        [Pkg] = [
            P
         || P <- maps:get(~"packages", ocibuild_json:decode(Json)),
            maps:get(~"name", P) =:= ~"dep"
        ],
        maps:get(~"downloadLocation", Pkg)
    end,
    [
        ?_assertEqual(
            ~"https://repo.hex.pm/tarballs/dep-1.2.3.tar", Loc(~"hex", ~"1.2.3")
        ),
        ?_assertEqual(
            ~"https://repo.hex.pm/tarballs/dep-1.2.3.tar", Loc(~"hexpm", ~"1.2.3")
        ),
        ?_assertEqual(
            ~"git+https://github.com/o/dep.git@abc123",
            Loc(~"https://github.com/o/dep.git", ~"abc123")
        ),
        ?_assertEqual(
            ~"git+https://github.com/o/dep.git@abc123",
            Loc(~"https://github.com/o/dep", ~"abc123")
        ),
        ?_assertEqual(
            ~"git+ssh://git@github.com/o/dep.git@v1",
            Loc(~"git@github.com:o/dep.git", ~"v1")
        ),
        ?_assertEqual(
            ~"git+https://github.com/o/dep.git",
            Loc(~"https://github.com/o/dep.git", ~"unknown")
        ),
        ?_assertEqual(~"NOASSERTION", Loc(~"unknown", ~"unknown"))
    ].

%%%===================================================================
%%% Supplier Tests
%%%===================================================================

supplier_test_() ->
    Deps = [
        #{name => ~"h", version => ~"1.0.0", source => ~"hex"},
        #{name => ~"g", version => ~"abc", source => ~"https://github.com/o/g.git"},
        #{name => ~"u", version => ~"unknown", source => ~"unknown"}
    ],
    Opts = #{
        app_name => ~"myapp",
        app_version => ~"1.0.0",
        base_image => {~"ghcr.io", ~"myorg/base", ~"v1"}
    },
    {ok, Json} = ocibuild_sbom:generate(Deps, Opts),
    Pkgs = maps:get(~"packages", ocibuild_json:decode(Json)),
    Supplier = fun(Name) ->
        [P] = [P || P <- Pkgs, maps:get(~"name", P) =:= Name],
        maps:get(~"supplier", P)
    end,
    [
        ?_assertEqual(~"Organization: hex.pm", Supplier(~"h")),
        ?_assertEqual(~"Organization: github.com", Supplier(~"g")),
        ?_assertEqual(~"NOASSERTION", Supplier(~"u")),
        ?_assertEqual(~"Organization: ghcr.io", Supplier(~"base"))
    ].

app_supplier_test_() ->
    Supplier = fun(Vendor) ->
        Opts = #{app_name => ~"myapp", app_version => ~"1.0.0", vendor => Vendor},
        {ok, Json} = ocibuild_sbom:generate([], Opts),
        [Pkg] = maps:get(~"packages", ocibuild_json:decode(Json)),
        maps:get(~"supplier", Pkg)
    end,
    [
        ?_assertEqual(~"Organization: Intility", Supplier(~"Intility")),
        ?_assertEqual(~"Organization: Intility", Supplier(~"  Intility ")),
        ?_assertEqual(~"Person: Ada", Supplier(~"Person: Ada")),
        ?_assertEqual(~"Organization: Intility", Supplier(~"Organization: Intility")),
        ?_assertEqual(~"NOASSERTION", Supplier(~"")),
        ?_assertEqual(~"NOASSERTION", Supplier(undefined))
    ].

%%%===================================================================
%%% SPDX ID Collision Tests
%%%===================================================================

id_collisions_test() ->
    %% foo_bar and foo-bar sanitize to the same ID; erts and base_image hit reserved IDs;
    %% "myapp" is the app's own ID; "dup" is listed twice.
    Dep = fun(N) -> #{name => N, version => ~"1.0.0", source => ~"hex"} end,
    Names = [~"foo_bar", ~"foo-bar", ~"erts", ~"base_image", ~"myapp", ~"dup", ~"dup"],
    Opts = #{
        app_name => ~"myapp",
        app_version => ~"1.0.0",
        erts_version => ~"14.2",
        base_image => {~"ghcr.io", ~"myorg/base", ~"v1"}
    },
    {ok, Json} = ocibuild_sbom:generate([Dep(N) || N <- Names], Opts),
    Sbom = ocibuild_json:decode(Json),
    Pkgs = maps:get(~"packages", Sbom),
    Ids = [maps:get(~"SPDXID", P) || P <- Pkgs],
    %% every ID is unique, and the repeated "dup" produced one package
    ?assertEqual(length(Ids), length(lists:uniq(Ids))),
    ?assertEqual(1, length([P || P <- Pkgs, maps:get(~"name", P) =:= ~"dup"])),
    %% the real ERTS package keeps the reserved ID
    [Erts] = [P || P <- Pkgs, maps:get(~"SPDXID", P) =:= ~"SPDXRef-Package-erts"],
    ?assertEqual(~"erts", maps:get(~"name", Erts)),
    ?assertEqual(~"14.2", maps:get(~"versionInfo", Erts)),
    %% every relationship refers to a package in the document
    Known = sets:from_list([~"SPDXRef-DOCUMENT" | Ids]),
    lists:foreach(
        fun(R) ->
            ?assert(sets:is_element(maps:get(~"spdxElementId", R), Known)),
            ?assert(sets:is_element(maps:get(~"relatedSpdxElement", R), Known))
        end,
        maps:get(~"relationships", Sbom)
    ).

id_collisions_are_deterministic_test() ->
    Deps = [#{name => N, version => ~"1", source => ~"hex"} || N <- [~"foo_bar", ~"foo-bar"]],
    Opts = #{app_name => ~"myapp", app_version => ~"1.0.0"},
    Ids = fun() ->
        {ok, Json} = ocibuild_sbom:generate(Deps, Opts),
        [maps:get(~"SPDXID", P) || P <- maps:get(~"packages", ocibuild_json:decode(Json))]
    end,
    ?assertEqual(
        [~"SPDXRef-Package-myapp", ~"SPDXRef-Package-foo-bar", ~"SPDXRef-Package-foo-bar-2"],
        Ids()
    ),
    ?assertEqual(Ids(), Ids()).

oci_purl_test_() ->
    Purl = fun(Digest) ->
        Opts = #{
            app_name => ~"myapp",
            app_version => ~"1.0.0",
            base_image => {~"ghcr.io", ~"myorg/base", ~"v1"},
            base_digest => Digest
        },
        {ok, Json} = ocibuild_sbom:generate([], Opts),
        [Base] = [
            P
         || P <- maps:get(~"packages", ocibuild_json:decode(Json)),
            maps:get(~"SPDXID", P) =:= ~"SPDXRef-Package-base-image"
        ],
        [Ref] = maps:get(~"externalRefs", Base),
        maps:get(~"referenceLocator", Ref)
    end,
    [
        ?_assertEqual(
            ~"pkg:oci/base@sha256%3Aabc123?repository_url=ghcr.io/myorg/base&tag=v1",
            Purl(~"sha256:abc123")
        ),
        ?_assertEqual(
            ~"pkg:oci/base?repository_url=ghcr.io/myorg/base&tag=v1", Purl(undefined)
        )
    ].

%%%===================================================================
%%% Checksum Tests
%%%===================================================================

checksum_test_() ->
    Sum = ~"ABCDEF0123456789abcdef0123456789ABCDEF0123456789abcdef0123456789",
    Checksums = fun(Extra) ->
        Dep = maps:merge(#{name => ~"dep", version => ~"1.0.0", source => ~"hex"}, Extra),
        {ok, Json} = ocibuild_sbom:generate([Dep], #{app_name => ~"myapp", app_version => ~"1"}),
        [P] = [
            P
         || P <- maps:get(~"packages", ocibuild_json:decode(Json)),
            maps:get(~"name", P) =:= ~"dep"
        ],
        maps:get(~"checksums", P, none)
    end,
    [
        ?_assertEqual(
            [
                #{
                    ~"algorithm" => ~"SHA256",
                    ~"checksumValue" => ~"abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789"
                }
            ],
            Checksums(#{checksum => Sum})
        ),
        ?_assertEqual(none, Checksums(#{})),
        ?_assertEqual(none, Checksums(#{checksum => ~"tooshort"})),
        ?_assertEqual(none, Checksums(#{checksum => binary:copy(~"z", 64)}))
    ].

%%%===================================================================
%%% Relationship Tests
%%%===================================================================

sbom_runtime_rels_attached_to_app_test() ->
    %% ERTS and the base image hang off the app, so no package floats outside the tree
    Opts = #{
        app_name => ~"myapp",
        app_version => ~"1.0.0",
        erts_version => ~"14.2",
        otp_version => ~"27.0",
        base_image => {~"ghcr.io", ~"myorg/base", ~"v1"},
        base_digest => ~"sha256:abc123"
    },
    {ok, SbomJson} = ocibuild_sbom:generate([], Opts),
    Rels = maps:get(~"relationships", ocibuild_json:decode(SbomJson)),
    App = ~"SPDXRef-Package-myapp",
    ?assert(has_rel(Rels, App, ~"DEPENDS_ON", ~"SPDXRef-Package-erts")),
    ?assert(has_rel(Rels, App, ~"DEPENDS_ON", ~"SPDXRef-Package-base-image")),
    Types = [maps:get(~"relationshipType", R) || R <- Rels],
    ?assertNot(lists:member(~"RUNTIME_DEPENDENCY_OF", Types)),
    ?assertNot(lists:member(~"BUILD_TOOL_OF", Types)).

has_rel(Rels, From, Type, To) ->
    lists:any(
        fun(R) ->
            maps:get(~"spdxElementId", R) =:= From andalso
                maps:get(~"relationshipType", R) =:= Type andalso
                maps:get(~"relatedSpdxElement", R) =:= To
        end,
        Rels
    ).

sbom_relationships_test() ->
    %% Test that SBOM includes proper relationships
    Deps = [#{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"}],
    Opts = #{app_name => ~"myapp", app_version => ~"1.0.0"},
    {ok, SbomJson} = ocibuild_sbom:generate(Deps, Opts),
    Sbom = ocibuild_json:decode(SbomJson),
    Relationships = maps:get(~"relationships", Sbom),
    %% Should have DESCRIBES and DEPENDS_ON relationships
    ?assert(length(Relationships) >= 2),
    %% Check for DESCRIBES relationship
    DescribesRels = lists:filter(
        fun(R) -> maps:get(~"relationshipType", R) =:= ~"DESCRIBES" end,
        Relationships
    ),
    ?assertEqual(1, length(DescribesRels)).

%%%===================================================================
%%% OCI Referrer Manifest Tests
%%%===================================================================

sbom_referrer_manifest_test() ->
    %% Test build_referrer_manifest/4 generates valid OCI artifact manifest
    ArtifactDigest = ~"sha256:abc123",
    ArtifactSize = 1234,
    SubjectDigest = ~"sha256:def456",
    SubjectSize = 5678,

    Manifest = ocibuild_sbom:build_referrer_manifest(
        ArtifactDigest, ArtifactSize, SubjectDigest, SubjectSize
    ),

    %% Verify manifest structure
    ?assertEqual(2, maps:get(~"schemaVersion", Manifest)),
    ?assertEqual(~"application/vnd.oci.image.manifest.v1+json", maps:get(~"mediaType", Manifest)),
    ?assertEqual(~"application/spdx+json", maps:get(~"artifactType", Manifest)),

    %% Verify config (empty blob)
    Config = maps:get(~"config", Manifest),
    ?assertEqual(~"application/vnd.oci.empty.v1+json", maps:get(~"mediaType", Config)),
    ?assertEqual(2, maps:get(~"size", Config)),

    %% Verify layers
    [Layer] = maps:get(~"layers", Manifest),
    ?assertEqual(~"application/spdx+json", maps:get(~"mediaType", Layer)),
    ?assertEqual(ArtifactDigest, maps:get(~"digest", Layer)),
    ?assertEqual(ArtifactSize, maps:get(~"size", Layer)),
    LayerAnnotations = maps:get(~"annotations", Layer),
    ?assertEqual(~"sbom.spdx.json", maps:get(~"org.opencontainers.image.title", LayerAnnotations)),

    %% Verify subject
    Subject = maps:get(~"subject", Manifest),
    ?assertEqual(~"application/vnd.oci.image.manifest.v1+json", maps:get(~"mediaType", Subject)),
    ?assertEqual(SubjectDigest, maps:get(~"digest", Subject)),
    ?assertEqual(SubjectSize, maps:get(~"size", Subject)).

%%%===================================================================
%%% SPDX 2.3 mandatory package fields
%%%===================================================================

sbom_mandatory_package_fields_test() ->
    %% SPDX requires licenseConcluded, licenseDeclared and copyrightText
    %% on every package, even when filesAnalyzed is false.
    Deps = [#{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"}],
    Opts = #{
        app_name => ~"myapp",
        app_version => ~"1.0.0",
        erts_version => ~"14.0",
        otp_version => ~"26.0",
        base_image => {~"docker.io", ~"library/debian", ~"stable"}
    },
    {ok, SbomJson} = ocibuild_sbom:generate(Deps, Opts),
    Packages = maps:get(~"packages", ocibuild_json:decode(SbomJson)),
    %% app + cowboy + erts + base image
    ?assertEqual(4, length(Packages)),
    lists:foreach(
        fun(Pkg) ->
            lists:foreach(
                fun(Field) ->
                    ?assertEqual(~"NOASSERTION", maps:get(Field, Pkg, missing))
                end,
                [~"licenseConcluded", ~"licenseDeclared", ~"copyrightText"]
            )
        end,
        Packages
    ).

sbom_created_timestamp_format_test() ->
    %% SPDX requires YYYY-MM-DDThh:mm:ssZ (no fractional seconds, no offset)
    {ok, SbomJson} = ocibuild_sbom:generate([], #{app_name => ~"myapp"}),
    Created = maps:get(~"created", maps:get(~"creationInfo", ocibuild_json:decode(SbomJson))),
    ?assertMatch(
        {match, _},
        re:run(Created, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")
    ).

%%%===================================================================
%%% Dependency graph
%%%===================================================================

%% DEPENDS_ON edges as {From, To} pairs using package names without the SPDXRef prefix
depends_on_edges(Deps, Opts) ->
    {ok, SbomJson} = ocibuild_sbom:generate(Deps, Opts),
    Rels = maps:get(~"relationships", ocibuild_json:decode(SbomJson)),
    Strip = fun(<<"SPDXRef-Package-", Name/binary>>) -> Name end,
    lists:sort([
        {Strip(maps:get(~"spdxElementId", R)), Strip(maps:get(~"relatedSpdxElement", R))}
     || R <- Rels, maps:get(~"relationshipType", R) =:= ~"DEPENDS_ON"
    ]).

graph_deps() ->
    [
        #{
            name => ~"plug_cowboy",
            version => ~"2.7.0",
            source => ~"hex",
            deps => [~"cowboy"],
            direct => true
        },
        #{
            name => ~"cowboy",
            version => ~"2.10.0",
            source => ~"hex",
            deps => [~"ranch", ~"cowlib"],
            direct => false
        },
        #{name => ~"ranch", version => ~"1.8.0", source => ~"hex", deps => [], direct => false},
        #{name => ~"cowlib", version => ~"2.12.0", source => ~"hex", deps => [], direct => false}
    ].

sbom_graph_direct_and_transitive_test() ->
    %% Only direct deps hang off the app; transitive deps hang off their parents
    ?assertEqual(
        [
            {~"cowboy", ~"cowlib"},
            {~"cowboy", ~"ranch"},
            {~"myapp", ~"plug-cowboy"},
            {~"plug-cowboy", ~"cowboy"}
        ],
        depends_on_edges(graph_deps(), #{app_name => ~"myapp"})
    ).

sbom_graph_infers_roots_without_direct_flags_test() ->
    %% Adapters that know the graph but not the direct deps: roots are the
    %% packages that no other package depends on
    Deps = [maps:remove(direct, D) || D <- graph_deps()],
    ?assertEqual(
        [
            {~"cowboy", ~"cowlib"},
            {~"cowboy", ~"ranch"},
            {~"myapp", ~"plug-cowboy"},
            {~"plug-cowboy", ~"cowboy"}
        ],
        depends_on_edges(Deps, #{app_name => ~"myapp"})
    ).

sbom_graph_flat_without_graph_info_test() ->
    %% Adapters that provide no graph data keep the flat behaviour
    Deps = [
        #{name => ~"cowboy", version => ~"2.10.0", source => ~"hex"},
        #{name => ~"ranch", version => ~"1.8.0", source => ~"hex"}
    ],
    ?assertEqual(
        [{~"myapp", ~"cowboy"}, {~"myapp", ~"ranch"}],
        depends_on_edges(Deps, #{app_name => ~"myapp"})
    ).

sbom_graph_ignores_unknown_and_self_edges_test() ->
    %% Optional or filtered-out deps are not packages in the SBOM, so no edge to them
    Deps = [
        #{
            name => ~"cowboy",
            version => ~"2.10.0",
            source => ~"hex",
            deps => [~"ranch", ~"cowboy", ~"not_shipped"],
            direct => true
        },
        #{name => ~"ranch", version => ~"1.8.0", source => ~"hex", deps => [], direct => false}
    ],
    ?assertEqual(
        [{~"cowboy", ~"ranch"}, {~"myapp", ~"cowboy"}],
        depends_on_edges(Deps, #{app_name => ~"myapp"})
    ).

sbom_graph_keeps_orphans_under_app_test() ->
    %% A git dep has no known deps, so its own dep has no parent edge. It must stay
    %% listed under the app instead of disappearing from the graph.
    Deps = [
        #{
            name => ~"gitdep",
            version => ~"abc123",
            source => ~"https://example.com/gitdep.git",
            direct => true
        },
        #{name => ~"gitdep_child", version => ~"1.0.0", source => ~"hex", direct => false}
    ],
    ?assertEqual(
        [{~"myapp", ~"gitdep"}, {~"myapp", ~"gitdep-child"}],
        depends_on_edges(Deps, #{app_name => ~"myapp"})
    ).

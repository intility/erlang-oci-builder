%%%-------------------------------------------------------------------
-module(ocibuild_sbom_tests).
-moduledoc """
Tests for ocibuild_sbom module - SPDX 2.2 SBOM generation.
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
    ?assertEqual(~"SPDX-2.2", maps:get(~"spdxVersion", Sbom)),
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
%%% Relationship Tests
%%%===================================================================

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
